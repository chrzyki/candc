/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

namespace NLP {
  namespace CCG {

    class SuperCat;

    class Printer {
    protected:
      void set(bool DONE, bool SUCCESS, const std::string &REASON, double BETA, ulong DICT_CUTOFF){
	nsentences += DONE;
	nparsed += SUCCESS;

	success = SUCCESS;

	reason = REASON;
	beta = BETA;
	dict_cutoff = DICT_CUTOFF;

	logderivs = 0;
	nequiv = 0;
	ntotal = 0;
      }

      virtual void unary(Sentence &sent){}
      virtual void derivation(const SuperCat *sc, Sentence &sent){}
      virtual void lexical(Sentence &sent){}
    public:
      Categories &cats;

      ulong nsentences;
      ulong nparsed;

      bool success;

      std::string reason;
      double beta;
      ulong dict_cutoff;

      double logderivs;
      ulong nequiv;
      ulong ntotal;

      float coverage(void) const { return nparsed*100.0/nsentences; }

      Printer(Categories &cats)
	: cats(cats),
	  nsentences(0), nparsed(0),
	  success(false),
	  logderivs(0), nequiv(0), ntotal(0){}

      virtual ~Printer(void){ /* do nothing */ }

      virtual void header(const std::string &preface){}
      virtual void footer(void){}

      virtual void parsed(const SuperCat *root, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(true, true, "parsed", BETA, DICT_CUTOFF);

	sent.cats.clear();

	if(root)
	  derivation(root, sent);
	else
	  unary(sent);

	lexical(sent);
      }

      virtual void stats(const double LOGDERIVS, const ulong NEQUIV, const ulong NTOTAL){
	logderivs = LOGDERIVS;
	nequiv = NEQUIV;
	ntotal = NTOTAL;
      }

      virtual void attempted(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(false, false, REASON, BETA, DICT_CUTOFF);
      }

      virtual void failed(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(true, false, REASON, BETA, DICT_CUTOFF);
      }

      virtual void error(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(true, false, REASON, BETA, DICT_CUTOFF);
      }
    };

  }
}
