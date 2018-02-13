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

  namespace Model {

    using namespace NLP::Config;

    class Info: public Cfg {
    public:
      Op<ulong> nklasses;
      Op<ulong> nevents;
      Op<ulong> ncontexts;
      Op<ulong> nfeatures;
      Op<ulong> nattributes;

      Info(void);
      Info(const std::string &filename);
    };

    class Model: public Cfg {
    public:
      const OpPath &path;

      Op<std::string> comment;
      Op<std::string> data;
      Op<std::string> solver;
      Op<std::string> update;
      Op<double> sigma;
      Op<ulong> niterations;

      OpPath weights;

      std::string info(void) const { return derived_path(path, "info"); }
      std::string features(void) const { return derived_path(path, "features"); }
      std::string attributes(void) const { return derived_path(path, "attributes"); }
      std::string contexts(void) const { return derived_path(path, "contexts"); }
      std::string klasses(void) const { return derived_path(path, "classes"); }
      std::string lexicon(void) const { return derived_path(path, "lexicon"); }

      Model(const std::string &name, const OpPath &path, double SIGMA, ulong NITER,
	    Flags flags = SHOW_ALL, ushort order = 255);

      virtual void check(void);

      virtual void writeln_help(std::ostream &out, std::string prefix = "", bool full = false) const {
	out << '\n';
	Cfg::writeln_help(out, prefix, full);
      }
    };

    class Config: public Directory {
    public:
      typedef uchar Mode;
      const static Mode TRAIN = 1 << 0;
      const static Mode ESTIMATE = 1 << 1;
      const static Mode DECODE = 1 << 2;

      Model model;

      Config(const std::string &name, const std::string &desc,
	     const OpPath *base, Mode mode, double SIGMA, ulong NITER);
    };

  }
}
