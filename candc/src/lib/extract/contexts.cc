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

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "extract/contexts.h"
#include "share.h"

using namespace std;

namespace NLP { namespace Extract {

using namespace HashTable;

class _ContextEntry {
public:
  static Hash calc_hash(Tag klass, const Context &context){
    Hash hash(0ul);
    for(Context::const_iterator i = context.begin(); i != context.end(); ++i)
      hash += *i;
    hash += klass.value();
    return hash;
  }

  _ContextEntry *next;
  ulong freq;
  Hash hash;
  Tag klass;
  Context context;

  _ContextEntry(Tag klass, const Context &context, NLP::Hash hash, _ContextEntry *next)
    : next(next), freq(1), hash(hash), klass(klass),
      context(context.begin(), context.end()){}
  ~_ContextEntry(void){}

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *pool) { /* do nothing */ }

  static _ContextEntry *create(Pool *pool, void *,
			       ulong, NLP::Hash, _ContextEntry *next){ return 0; }

  bool equal(Tag klass, NLP::Hash hash, const Context &context) const {
    return klass == this->klass && hash == this->hash & context == this->context;
  }

  _ContextEntry *find(Tag klass, NLP::Hash hash, const Context &context){
    for(_ContextEntry *l = this; l; l = l->next)
      if(l->equal(klass, hash, context))
        return l;

    return 0;
  }

  ostream &save(ostream &stream) const {
    stream << freq << ' ' << klass.value() << ' ' << context.size();
    for(Context::const_iterator i = context.begin(); i != context.end(); ++i)
      stream << ' ' << *i;
    return stream;
  }

  // count the hash table chain length
  ulong nchained(void){
    return next ? next->nchained() + 1 : 1;
  }
};

// use a Count hash table, with a large memory and string pool
typedef Ordered<_ContextEntry, void *, MEDIUM, MEDIUM> _ImplBase;

template <class E>
class ContextCmp {
public:
  bool operator ()(const E *const e1, const E *const e2){
    return e1->context < e2->context;
  }
};

class Contexts::_Impl: public _ImplBase, public Shared {
public:
  string PREFACE;

  _Impl(const string &name): _ImplBase(name){}
  virtual ~_Impl(void){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->~Entry();
  }

  void add(Tag klass, const Context &context){
    NLP::Hash hash = Entry::calc_hash(klass, context);
    ulong bucket = hash % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(klass, hash, context);
    if(entry){
      entry->freq++;
      return;
    }

    entry = new (pool_) Entry(klass, context, hash, buckets_[bucket]);
    buckets_[bucket] = entry;
    entries.push_back(entry);
    ++size;
  }

  void sort_by_attributes(void){
    std::sort(entries.begin(), entries.end(), ContextCmp<Entry>());
  }

  void save(ostream &out){
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->save(out) << '\n';
  }
};

Contexts::Contexts(void):
  _impl(new _Impl("contexts")), PREFACE(_impl->PREFACE){}

Contexts::Contexts(const Contexts &other):
  _impl(share(other._impl)), PREFACE(_impl->PREFACE){}

Contexts &
Contexts::operator=(const Contexts &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Contexts::~Contexts(void){
  release(_impl);
}

size_t
Contexts::size(void) const {
  return _impl->size;
}

void
Contexts::save(const string &filename, const string &preface) const {
  ofstream stream(filename.c_str());
  if(!stream)
    throw IOException("could not open " + _impl->name + " file for writing", filename);

  stream << preface << endl;

  _impl->save(stream);
}

void
Contexts::add(Tag klass, const Context &context){
  _impl->add(klass, context);
}

void
Contexts::sort_by_attributes(void){
  _impl->sort_by_attributes();
}

} }

