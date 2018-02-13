// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Extract::Context and NLP::Extract::Attributes
// storage for contexts, and features and attributes in
// the model extraction process

#include "base.h"

#include "config/config.h"

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "model/types.h"

#include "extract/feature.h"
#include "extract/attributes.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Extract {

using namespace HashTable;
using namespace Model;

// custom hash function and hash table entry for storing attributes
// this stores the tuple (type, value, freq), the attribute index
// which becomes the attribute id.  It also stores a vector of the
// features which have this attribute value.
class _AttributeEntry {
private:
  _AttributeEntry(const char *type, _AttributeEntry *next)
    : next(next), index(0), value(0), type(type) {}
  ~_AttributeEntry(void){}

  void *operator new(size_t size, Pool *pool, size_t len){
    return (void *)pool->alloc(size + aligned_size(len, sizeof(_AttributeEntry)));
  }

  void operator delete(void *, Pool *, size_t) { /* do nothing */ }
public:
  static Hash hash(const char *type, const string &str){
    Hash hash(type);
    hash += ' ';
    hash += str;
    return hash;
  }

  static _AttributeEntry *create(Pool *pool, const string &str,
				 ulong, NLP::Hash, _AttributeEntry *next){ return 0; }
  static _AttributeEntry *create(Pool *pool, const char *type,
				 const string &str, _AttributeEntry *next){
    _AttributeEntry *entry = new (pool, str.size()) _AttributeEntry(type, next);
    strcpy(entry->str, str.c_str());
    return entry;
  }

  _AttributeEntry *next;
  ulong index;
  ulong value;
  Features features;
  const char *type;
  char str[0];

  void insert(Tag tag){
    features.push_back(Feature(tag, 1));
    ++value;
  }
  void inc(Tag tag){
    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      if(i->tag == tag){
        ++value;
        ++i->freq;
        return;
      }

    insert(tag);
  }

  bool equal(const char *type, const string &str) const {
    return type == this->type && str == this->str;
  }

  _AttributeEntry *find(const char *type, const string &str){
    for(_AttributeEntry *l = this; l; l = l->next)
      if(l->equal(type, str))
        return l;

    return 0;
  }

  bool find(const char *type, const string &str, ulong &id) const {
    for(const _AttributeEntry *l = this; l; l = l->next)
      if(l->equal(type, str) && l->value){
        id = l->index;
        return true;
      }

    return false;
  }

  // apply a minimum frequency cutoff to the features in this Attribute
  // if the feature frequency < freq, the frequency is set to zero
  // and so it is ignored in later processing
  bool cutoff(ulong freq){
    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      if(i->freq < freq){
        value -= i->freq;
        i->freq = 0;
      }

    return value == 0;
  }

  // dump the attribute information
  void save_attributes(ostream &stream) const {
    stream << type << ' ' << str << ' ' << value << '\n';
  }

  // sort the features by class and then dump
  void save_features(ostream &stream){
    sort(features.begin(), features.end(), FeatureCmp());
    for(Features::const_iterator i = features.begin(); i != features.end(); ++i)
      // skip features with frequency zero (i.e. those below the cutoff)
      if(i->freq)
        stream << i->tag.value() << ' ' << index << ' ' << i->freq << '\n';
  }

  // calculate the total number of features by type
  ulong nfeatures(void) const {
    ulong total = 0;
    for(Features::const_iterator i = features.begin(); i != features.end(); ++i)
      if(i->freq)
        total++;
    return total;
  }

  // count the hash table chain length
  ulong nchained(void){
    return next ? next->nchained() + 1 : 1;
  }
};

// use the hash table that supports countable entries
typedef Ordered<_AttributeEntry, const string &, MEDIUM, LARGE> _ImplBase;

// private implementation, which is a shared hash table
class Attributes::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const string &name)
    : _ImplBase(name){}
  virtual ~_Impl(void) { /* do nothing */ };

  void add(const char *type, const string &value, Tag tag){
    ulong bucket = Entry::hash(type, value) % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(type, value);
    if(entry)
      return entry->inc(tag);

    entry = Entry::create(pool_, type, value, buckets_[bucket]);
    buckets_[bucket] = entry;
    entries.push_back(entry);
    ++size;

    entry->insert(tag);
  }

  bool find(const char *type, const string &value, ulong &id){
    Hash hash = Entry::hash(type, value);
    return buckets_[hash % NBUCKETS_]->find(type, value, id);
  }

  // eliminate any features with frequency less than freq
  void apply_cutoff(const ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->cutoff(freq))
        (*i) = 0;
  }

  // eliminate any features of a given type with a frequency less than freq
  void apply_cutoff(const char *type, const ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->type == type && (*i)->cutoff(freq))
        (*i) = 0;
  }

  // eliminate any features of a given type with frequency less than freq
  // and any other features with a frequency less than a default value def
  void apply_cutoff(const char *type, const ulong freq, const ulong def){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->type == type){
        if((*i)->cutoff(freq))
          (*i) = 0;
      }else if((*i)->cutoff(def))
        (*i) = 0;
  }

  // dump out the current list of attributes to a given filename
  void save_attributes(const string &filename, const string &PREFACE) const {
    ofstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open attributes file for writing", filename);
    stream << PREFACE << '\n';
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->save_attributes(stream);
  }

  // dump out the current list of features to a given filename
  void save_features(const string &filename, const string &PREFACE){
    ofstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open features file for writing", filename);

    stream << PREFACE << '\n';
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->save_features(stream);
  }

  ulong nfeatures(void) const {
    ulong total = 0;
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      total += (*i)->nfeatures();
    return total;
  }
};

// public wrappers for the private implementation

Attributes::Attributes(const string &name): _impl(new _Impl(name)) {}
Attributes::Attributes(const Attributes &other): _impl(share(other._impl)){}

Attributes &
Attributes::operator=(const Attributes &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Attributes::~Attributes(void){
  release(_impl);
}

const string
Attributes::name(void) const {
  return _impl->name;
}

size_t
Attributes::size(void) const {
  return _impl->entries.size();
}

void
Attributes::operator()(Context &context, const Type &type, const string &value) const {
  ulong id;
  if(_impl->find(type.id, value, id))
    context.push_back(id);
}

void
Attributes::operator()(Context &context, const Type &type, const string &v1,
		       const string &v2) const {
  ulong id;
  if(_impl->find(type.id, v1 + ' ' + v2, id))
    context.push_back(id);
}

void
Attributes::operator()(Tag klass, const Type &type, const string &value){
  _impl->add(type.id, value, klass);
}

void
Attributes::operator()(Tag klass, const Type &type, const string &v1, const string &v2){
  _impl->add(type.id, v1 + ' ' + v2, klass);
}

ulong
Attributes::nfeatures(void) const {
  return _impl->nfeatures();
}

void
Attributes::apply_cutoff(ulong freq){
  _impl->apply_cutoff(freq);
}

void
Attributes::apply_cutoff(const Type &type, ulong freq){
  _impl->apply_cutoff(type.id, freq);
}

void
Attributes::apply_cutoff(const Type &type, ulong freq, ulong def){
  _impl->apply_cutoff(type.id, freq, def);
}

void
Attributes::save(const string &attributes, const string &features, const string &PREFACE){
  // eliminate zero entries
  _impl->compress();
  // put the most frequent attributes first
  // and setup the attribute index values
  _impl->sort_by_rev_value();
  // dump out the attributes
  _impl->save_attributes(attributes, PREFACE);
  // dump out the features
  _impl->save_features(features, PREFACE);
}

} }
