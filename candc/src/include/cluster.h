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
  namespace Cluster {

    extern int rank;
    extern int size;
    extern std::string rank_str;
    extern std::string size_str;
    extern std::string processor;

    void init(int &argc, char **&argv);
    void finalize(void);

  }
}
