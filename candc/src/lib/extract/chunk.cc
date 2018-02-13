// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Extract::Chunk
// extracts features from chunktagged training data
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Tagger::Chunk for tagging

#include "extract/_baseimpl.h"
#include "extract/tagger.h"

#include "tagger/tagsetdict.h"
#include "tagger/chunk.h"
#include "extract/chunk.h"

using namespace std;

namespace NLP { namespace Extract {

// private implementation, which is shared
class Chunk::_Impl: public _TaggerImpl {
protected:
  template <class TC>
  void _add_surrounding(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding2(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_history(TC &tc, const Sentence &sent, ulong i);

  template <class TC>
  void _generate(TC &tc, const Sentence &sent, ulong i);

  void _generate_counts(const Sentence &sent);
  void _generate_features(const Sentence &sent);
  void _generate_contexts(const Sentence &sent);
  void _make_unknowns(void) const;

  void _pass1(NLP::IO::Reader &reader);
public:
  NLP::Tagger::Chunk::Config &cfg;

  Lexicon poscounts;	// POS tags and frequency counts

  _Impl(NLP::Tagger::Chunk::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  ~_Impl(void);
};

// count surrounding word features (2 words to left and right)
// count surrounding POS tag features (2 tags to left and right)
template <class TC>
void
Chunk::_Impl::_add_surrounding(TC &tc, const Sentence &sent, ulong i){
  if(i > 0){
    attributes(tc, Types::pw, sent.words[i - 1]);
    attributes(tc, Types::pt, sent.pos[i - 1]);
    if(i > 1){
      attributes(tc, Types::ppw, sent.words[i - 2]);
      attributes(tc, Types::ppt, sent.pos[i - 2]);
    }else{
      attributes(tc, Types::ppw, SENTINEL);
      attributes(tc, Types::ppt, SENTINEL);
    }
  }else{
    attributes(tc, Types::pw, SENTINEL);
    attributes(tc, Types::ppw, SENTINEL);

    attributes(tc, Types::pt, SENTINEL);
    attributes(tc, Types::ppt, SENTINEL);
  }

  size_t last = sent.words.size() - 1;
  if(i < last){
    attributes(tc, Types::nw, sent.words[i + 1]);
    attributes(tc, Types::nt, sent.pos[i + 1]);
    --last;
    if(i < last){
      attributes(tc, Types::nnw, sent.words[i + 2]);
      attributes(tc, Types::nnt, sent.pos[i + 2]);
    }else{
      attributes(tc, Types::nnw, SENTINEL);
      attributes(tc, Types::nnt, SENTINEL);
    }
  }else{
    attributes(tc, Types::nw, SENTINEL);
    attributes(tc, Types::nnw, SENTINEL);

    attributes(tc, Types::nt, SENTINEL);
    attributes(tc, Types::nnt, SENTINEL);
  }
}

// count surrounding2 POS tag features (2 tags to left and right)
template <class TC>
void
Chunk::_Impl::_add_surrounding2(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.pos.size() - 1;
  if(i > 0){
    attributes(tc, Types::ptt, sent.pos[i - 1], sent.pos[i]);
    if(i > 1)
      attributes(tc, Types::pptptb, sent.pos[i - 2], sent.pos[i - 1]);
    else
      attributes(tc, Types::pptptb, SENTINEL, sent.pos[i - 1]);

    if(i < last)
      attributes(tc, Types::ptntb, sent.pos[i - 1], sent.pos[i + 1]);
    else
      attributes(tc, Types::ptntb, sent.pos[i - 1], SENTINEL);
  }else{
    attributes(tc, Types::pttb, SENTINEL, sent.pos[i]);
    attributes(tc, Types::pptptb, SENTINEL2);

    if(i < last)
      attributes(tc, Types::ptntb, SENTINEL, sent.pos[i + 1]);
    else
      attributes(tc, Types::ptntb, SENTINEL2);
  }

  if(i < last){
    attributes(tc, Types::tntb, sent.pos[i], sent.pos[i + 1]);
    --last;
    if(i < last)
      attributes(tc, Types::ntnntb, sent.pos[i + 1], sent.pos[i + 2]);
    else
      attributes(tc, Types::ntnntb, sent.pos[i + 1], SENTINEL);
  }else{
    attributes(tc, Types::tntb, sent.pos[i], SENTINEL);
    attributes(tc, Types::ntnntb, SENTINEL2);
  }
}

// count previously assigned tag features
// for the previous tag and the bigram (previous tag, prev-prev tag)
template <class TC>
void
Chunk::_Impl::_add_history(TC &tc, const Sentence &sent, ulong i){
  static std::string tmp;
  if(i > 0){
    if(!klasses[sent.chunks[i - 1]])
      return;

    attributes(tc, Types::pst, sent.chunks[i - 1]);

    if(i > 1){
      if(!klasses[sent.chunks[i - 2]])
        return;

      attributes(tc, Types::ppst, sent.chunks[i - 2], sent.chunks[i - 1]);
    }else
      attributes(tc, Types::ppst, SENTINEL, sent.chunks[i - 1]);
  }else{
    attributes(tc, Types::pst, SENTINEL);
    attributes(tc, Types::ppst, SENTINEL2);
  }
}

// count the number of tags, words and word/tag pairs in the given sentence
void
Chunk::_Impl::_generate_counts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    counts.add(sent.chunks[i]);
    lexicon.add(sent.words[i], 1);
    poscounts.add(sent.pos[i], 1);
    tagdict.add(sent.words[i] + ' ' + sent.chunks[i], 1);
  }
  nevents += sent.words.size();
}

template <class TC>
void
Chunk::_Impl::_generate(TC &tc, const Sentence &sent, ulong i){
  // rare word features only switch on for infrequent words
  // with frequency less than config.rare
  attributes(tc, Types::w, sent.words[i]);
  attributes(tc, Types::t, sent.pos[i]);

  _add_surrounding(tc, sent, i);
  _add_surrounding2(tc, sent, i);
  _add_history(tc, sent, i);
}

// count the features for each training instance in the given sentence
void
Chunk::_Impl::_generate_features(const Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    Tag klass = klasses[sent.chunks[i]];
    if(!klass)
      continue;

    _generate(klass, sent, i);
  }
  nevents += sent.words.size();
}

// add the features for each training instance to the context
// and dump it model/contexts for the given sentence
void
Chunk::_Impl::_generate_contexts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    context.resize(0);

    Tag klass = klasses[sent.chunks[i]];
    if(!klass)
      continue;

    _generate(context, sent, i);
    
    if(context.size() > 0){
      std::sort(context.begin(), context.end());
      contexts.add(klass, context);
    }
  }
  nevents += sent.words.size();
}

void
Chunk::_Impl::_make_unknowns(void) const {
  ofstream stream(cfg.unknowns().c_str());
  if(!stream)
    throw NLP::IOException("could not open unknown_tags file for writing", cfg.unknowns());

  stream << PREFACE << '\n';
}

void
Chunk::_Impl::_pass1(NLP::IO::Reader &reader){
  _TaggerImpl::_pass1(reader);

  // POS tags are sorted alphabetically and dumped
  poscounts.sort_by_alpha();
  poscounts.save(cfg.postags(), PREFACE);
}

Chunk::_Impl::_Impl(NLP::Tagger::Chunk::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _TaggerImpl(cfg, PREFACE, VERBOSE), cfg(cfg), poscounts("poscount"){}

Chunk::_Impl::~_Impl(void) {}

Chunk::Chunk(NLP::Tagger::Chunk::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _impl(new _Impl(cfg, PREFACE, VERBOSE)){}

Chunk::Chunk(const Chunk &other): _impl(share(other._impl)){}

Chunk::~Chunk(void){
  release(_impl);
}

ulong Chunk::nevents(void) const { return _impl->nevents; };
ulong Chunk::ncontexts(void) const { return _impl->ncontexts; };

TagSet Chunk::tagset(void) const { return _impl->klasses; };
Lexicon Chunk::lexicon(void) const { return _impl->lexicon; };

void Chunk::extract(NLP::IO::Reader &reader){ _impl->extract(reader, true); }

} }
