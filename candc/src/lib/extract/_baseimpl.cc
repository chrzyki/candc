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

using namespace std;

namespace NLP { namespace Extract {

// count the words and tags, and build the tag dictionary
// dump to //lexicon, //postags, //classes and //tagdict
void
_BaseImpl::_pass1(NLP::IO::Reader &reader){
  _apply(reader, "pass 1", this, &_BaseImpl::_generate_counts);

  // classes are sorted alphabetically and dumped
  counts.sort_by_alpha();
  counts.save(cfg.model.klasses(), PREFACE);

  // lexicon is sorted alphabetically and dumped
  lexicon.sort_by_alpha();
  lexicon.save(cfg.model.lexicon(), PREFACE);
}

// count the number of features and attributes extracted from the training data
// dump out to model/features and model/attributes
void
_BaseImpl::_pass2(NLP::IO::Reader &reader){
  klasses.load(cfg.model.klasses());

  _apply(reader, "pass 2", this, &_BaseImpl::_generate_features);

  // apply the feature cutoff and dump the attributes and features
  _apply_cutoffs();
  attributes.save(cfg.model.attributes(), cfg.model.features(), PREFACE);
}

// extract features, translate them to attribute IDs for context vectors
// dump context vectors to model/contexts and compress to merge duplicates
void
_BaseImpl::_pass3(NLP::IO::Reader &reader){
  _apply(reader, "pass 3", this, &_BaseImpl::_generate_contexts);
}

// save out the contexts file
// freq klass nattributes attribute1 attribute2 ...
void
_BaseImpl::_save_contexts(void){
  contexts.sort_by_attributes();
  contexts.save(cfg.model.contexts(), PREFACE);
  ncontexts = contexts.size();
}

// run the three passes and then save the information about the size
// of the model (number of classes, attributes, features etc)
void
_BaseImpl::extract(NLP::IO::Reader &reader, bool MKDIR){
  if(MKDIR)
    Port::make_directory(cfg.path());

  _pass1(reader);
  _pass2(reader);
  _pass3(reader);
  _save_contexts();
  _make_unknowns();

  NLP::Model::Info info;

  info.nklasses.set_value(klasses.size());
  info.nattributes.set_value(attributes.nattributes());
  info.nfeatures.set_value(attributes.nfeatures());
  info.nevents.set_value(nevents);
  info.ncontexts.set_value(ncontexts);

  info.write_config(cfg.model.info(), PREFACE);
  cfg.write_config(cfg.config(), PREFACE);
}

_BaseImpl::_BaseImpl(NLP::Model::Config &cfg,
		     const std::string &PREFACE, bool VERBOSE)
  : cfg(cfg), VERBOSE(VERBOSE), PREFACE(PREFACE),
    NONE(None::str), nevents(0), ncontexts(0),
    lexicon("lexicon"),
    klasses("klasses"),
    attributes("attributes"),
    counts("counts"){}

_BaseImpl::~_BaseImpl(void){}

} }
