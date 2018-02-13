// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "extract/_baseimpl.h"
#include "extract/tagger.h"

using namespace std;

namespace NLP { namespace Extract {

// count the words and tags, and build the tag dictionary
// dump to //lexicon, //postags, //classes and //tagdict
void
_TaggerImpl::_pass1(NLP::IO::Reader &reader){
  _BaseImpl::_pass1(reader);

  // tag dictionary is sorted alphabetically on the word-tag pairs and dumped
  tagdict.sort_by_alpha();
  tagdict.save(cfg.tagdict(), PREFACE);
}

// count the number of features and attributes extracted from the training data
// dump out to model/features and model/attributes
void
_TaggerImpl::_apply_cutoffs(void){
  attributes.apply_cutoff(Types::w, cfg.cutoff_words(), cfg.cutoff_default());
}

_TaggerImpl::_TaggerImpl(NLP::Tagger::Tagger::Config &cfg,
			 const std::string &PREFACE, bool VERBOSE)
  : _BaseImpl(cfg, PREFACE, VERBOSE), cfg(cfg),
    SENTINEL(Sentinel::str), SENTINEL2(SENTINEL + ' ' + SENTINEL),
    tagdict("tagdict"){}

_TaggerImpl::~_TaggerImpl(void){}

// run the three passes and then save the information about the size
// of the model (number of classes, attributes, features etc)
void
_TaggerImpl::extract(NLP::IO::Reader &reader, bool MKDIR){
  _BaseImpl::extract(reader, MKDIR);
  _make_unknowns();
}


} }
