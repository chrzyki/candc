// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "tagger/_baseimpl.h"
#include "tagger/tagsetdict.h"
#include "tagger/super.h"

namespace NLP { namespace Tagger {

Super::Config::Config(const OpPath *base, Mode mode,
		      const std::string &name, const std::string &desc)
  : Tagger::Config(name, desc, base, mode, 0.811, 200),
    category_cutoff(*this, "category_cutoff", "the minimum frequency cutoff for categories", 10),
    postags(*this, "postags", "the POS tag set", "//postags", &path),
    posdict(*this, "posdict", "the POS tag dictionary file path", "//posdict", &path){}

typedef Tagger::Impl Base;

class Super::Impl: public Base {
public:
  const NLP::TagSet postags;
  TagSetDict posdict;

  Impl(Super::Config &cfg);
  virtual ~Impl(void);
protected:
  TagAttributes t_attribs;
  TagAttributes pt_attribs;
  TagAttributes ppt_attribs;
  TagAttributes nt_attribs;
  TagAttributes nnt_attribs;

  BiTagAttributes ptt_attribs;
  BiTagAttributes pptpt_attribs;
  BiTagAttributes ptnt_attribs;
  BiTagAttributes tnt_attribs;
  BiTagAttributes ntnnt_attribs;

  void add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const;
  void add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const;

  void reg_attributes(void);
  void create_unknowns(const Tagger::Config &cfg);
  void can_sentence(const Sentence &sent, State &state) const;
  const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const;
  void add_features(const State &state, ulong i, PDF &pdf) const;
  void unpack_tags(State &state, Sentence &sent) const;
  void unpack_mtags(State &state, Sentence &sent, double BETA) const;
};

Super::Impl::Impl(Super::Config &cfg)
  : Base("Super", cfg),
    postags("postags", cfg.postags()),
    posdict("posdict", cfg.posdict(), cfg.tagdict_min(), klasses, postags),
    t_attribs(postags), pt_attribs(postags), ppt_attribs(postags),
    nt_attribs(postags), nnt_attribs(postags),
    ptt_attribs(postags), pptpt_attribs(postags), ptnt_attribs(postags),
    tnt_attribs(postags), ntnnt_attribs(postags){
  create_model(cfg);
}

Super::Impl::~Impl(void){}

inline void
Super::Impl::add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(pt_attribs(tags[i - 1]), dist);
  _add_attribute(ppt_attribs(tags[i - 2]), dist);
  _add_attribute(nt_attribs(tags[i + 1]), dist);
  _add_attribute(nnt_attribs(tags[i + 2]), dist);
}

inline void
Super::Impl::add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(ptt_attribs(tags[i - 1], tags[i]), dist);
  _add_attribute(pptpt_attribs(tags[i - 2], tags[i - 1]), dist);
  _add_attribute(ptnt_attribs(tags[i - 1], tags[i + 1]), dist);
  _add_attribute(tnt_attribs(tags[i], tags[i + 1]), dist);
  _add_attribute(ntnnt_attribs(tags[i + 1], tags[i + 2]), dist);
}

void
Super::Impl::reg_attributes(void){
  Base::reg_attributes();

  registry.reg(Types::ppt, ppt_attribs);
  registry.reg(Types::pt, pt_attribs);
  registry.reg(Types::t, t_attribs);
  registry.reg(Types::nt, nt_attribs);
  registry.reg(Types::nnt, nnt_attribs);

  registry.reg(Types::pptptb, pptpt_attribs);
  registry.reg(Types::pttb, ptt_attribs);
  registry.reg(Types::ptntb, ptnt_attribs);
  registry.reg(Types::tntb, tnt_attribs);
  registry.reg(Types::ntnntb, ntnnt_attribs);

  registry.reg(Types::pst, pk_attribs);
  registry.reg(Types::ppst, ppkpk_attribs);
}

void
Super::Impl::create_unknowns(const Tagger::Config &cfg){
  // allow all supertags in the case where the POS tags is unknown
  unknown_klasses.reserve(klasses.size() - 2);
  for(ulong i = 2; i < klasses.size(); ++i)
    unknown_klasses.push_back(Tag(i));
}

void
Super::Impl::can_sentence(const Sentence &sent, State &state) const {
  if(sent.words.size() > maxwords)
    throw NLP::Exception("sentence length exceeds maximum number of words for supertagger");

  state.raws = sent.words;
  if(sent.words.size() != sent.pos.size())
    throw NLP::Exception("the number of words and POS tags in a sentence must be the same");
  lexicon.can(state.raws, state.words);
  postags.tag(sent.pos, state.pos);
}

const Tags &
Super::Impl::get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const {
  const Tags *tags = 0;
  if(state.words[i].freq() < DICT_CUTOFF)
    tags = &posdict[state.pos[i]];
  else
    tags = &tagdict[state.words[i]];

  if(!tags->size())
    tags = &unknown_klasses;
  return *tags;
}

void
Super::Impl::add_features(const State &state, ulong i, PDF &pdf) const {
  _add_attribute(w_attribs(Types::w, state.words[i]), pdf);
  _add_attribute(t_attribs(state.pos[i]), pdf);
  add_surrounding_words(state.words, i, pdf);
  add_surrounding_tags(state.pos, i, pdf);
  add_surrounding_bitags(state.pos, i, pdf);
}

void
Super::Impl::unpack_tags(State &state, Sentence &sent) const {
  _unpack_tags(state, sent.super, false);
}

void
Super::Impl::unpack_mtags(State &state, Sentence &sent, const double BETA) const {
  _unpack_mtags(state, sent.msuper, BETA, false);
}

Super::Super(Super::Config &cfg): Tagger(cfg, new Impl(cfg)){}
Super::Super(Super &other): Tagger(other){}
Super::~Super(void){}

TagSet
Super::postags(void) const {
  return dynamic_cast<const Super::Impl *>(impl_)->postags;
}

TagSetDict
Super::posdict(void) const {
  return dynamic_cast<const Super::Impl *>(impl_)->posdict;
}

} }
