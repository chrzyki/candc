// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "utils.h"

#include "hashtable/base.h"
#include "hashtable/frame.h"

#include "share.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/distance.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/supercat.h"
#include "parser/unify.h"

#include "parser/rule_instances.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace NLP::HashTable;
using namespace NLP::CCG;

class _RuleInstEntry {
public:
  static Hash hash(const Cat *cat1, const Cat *cat2){
    Hash h(cat1->rhash); // note rhash
    h += cat2->rhash;
    return h;
  }

  const Cat *_cat1;
  const Cat *_cat2;

  _RuleInstEntry *_next;
public:
  _RuleInstEntry(const Cat *cat1, const Cat *cat2, _RuleInstEntry *next):
    _cat1(cat1), _cat2(cat2), _next(next) {}
  ~_RuleInstEntry(void) {}

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *pool) { /* do nothing */ }

  const Cat *cat1(void) const { return _cat1; }
  const Cat *cat2(void) const { return _cat2; }

  bool equal(const Cat *cat1, const Cat *cat2){
    // rule_eq checks the feature but ignores variable features (X)
    return rule_eq(cat1, _cat1) && rule_eq(cat2, _cat2);
  }

  bool find(const Cat *cat1, const Cat *cat2){
    for(_RuleInstEntry *l = this; l; l = l->_next)
      if(l->equal(cat1, cat2))
        return true;

    return false;
  }

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  }
};

typedef Frame<_RuleInstEntry, MEDIUM, LARGE> _ImplBase;
class RuleInstances::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const string &name): _ImplBase(name) {}
  virtual ~_Impl(void) {}

  void insert(const Cat *cat1, const Cat *cat2){
    ulong bucket = Entry::hash(cat1, cat2) % _NBUCKETS;
    _buckets[bucket] = new (_ent_pool) Entry(cat1, cat2, _buckets[bucket]);
    ++size;
  }

  bool find(const Cat *cat1, const Cat *cat2) const {
    Hash hash = Entry::hash(cat1, cat2);
    return _buckets[hash % _NBUCKETS]->find(cat1, cat2);
  }
};

RuleInstances::RuleInstances(void): _impl(new _Impl("")) {}
RuleInstances::RuleInstances(RuleInstances &other): _impl(share(other._impl)) {}

RuleInstances::~RuleInstances(void){
  release(_impl);
}

size_t
RuleInstances::size(void) const {
  return _impl->size;
}

bool
RuleInstances::get(const Cat *cat1, const Cat *cat2) const {
  return _impl->find(cat1, cat2);
}

void
RuleInstances::insert(const Cat *cat1, const Cat *cat2) {
  _impl->insert(cat1, cat2);
}

} }
