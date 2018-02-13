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

    class PrinterFactory: public StreamPrinter {
    public:
      static void check(const std::string &name);
    protected:
      StreamPrinter *printer;

      virtual void unary(Sentence &sent){}
      virtual void derivation(const SuperCat *sc, Sentence &sent){}
      virtual void lexical(Sentence &sent){}

      StreamPrinter *create_printer(const std::string &name) const;
    public:
      PrinterFactory(const std::string &name, IO::Output &out,
		     IO::Log &log, Categories &cats,
		     const Format FORMAT);

      virtual ~PrinterFactory(void){
	delete printer;
      }

      virtual void header(const std::string &PREFACE){
	printer->header(PREFACE);
      }

      virtual void footer(void){
	printer->footer();
      }

      virtual void parsed(const SuperCat *sc, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(true, true, "parsed", BETA, DICT_CUTOFF);
	printer->parsed(sc, sent, BETA, DICT_CUTOFF);
      }

      virtual void stats(const double LOGDERIVS, const ulong NEQUIV, const ulong NTOTAL){
	Printer::stats(LOGDERIVS, NEQUIV, NTOTAL);
	printer->stats(LOGDERIVS, NEQUIV, NTOTAL);
      }

      virtual void attempted(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(false, false, REASON, BETA, DICT_CUTOFF);
	printer->attempted(REASON, sent, BETA, DICT_CUTOFF);
      }

      virtual void failed(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(false, false, REASON, BETA, DICT_CUTOFF);
	printer->failed(REASON, sent, BETA, DICT_CUTOFF);
      }
    };

  }
}
