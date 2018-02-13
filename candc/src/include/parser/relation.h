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

    class Relation {
    public:
      const char *cat;
      const ulong slot;  // slot number from the markedup file
      const ulong jslot; // slot number using Julia's scheme
      GRTemplate *gr;    // B&C grammatical relation format strings

      Relation(const char *cat, ulong slot, ulong jslot)
	: cat(cat), slot(slot), jslot(jslot), gr(0){}
      Relation(const Relation &other)
	: cat(other.cat), slot(other.slot), jslot(other.jslot),
	  gr(other.gr){}
      ~Relation(void){}

      void print_slot(std::ostream &out, const bool julia_slots) const {
	if(julia_slots)
	  out << cat << ' ' << jslot;
	else
	  out << cat << ' ' << slot;
      }
    };

  }
}
