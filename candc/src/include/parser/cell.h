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

    class Cell: public std::vector<SuperCat *> {
    public:
      Cell(void){}
      
      void add(const Cell &cell){ insert(end(), cell.begin(), cell.end()); }
      void add(SuperCat *sc){ push_back(sc); }
      void clear(void){ resize(0); }

    };

  }
}
