// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <mpi.h>
#include <string>
#include <sstream>

#include "cluster.h"

namespace NLP {
  namespace Cluster {

    int rank = 0;
    int size = 1;
    std::string rank_str;
    std::string size_str;
    std::string processor;

    void init(int &argc, char **&argv){
      int len;
      char buffer[MPI::MAX_PROCESSOR_NAME];

      MPI::Init(argc, argv);

      rank = MPI::COMM_WORLD.Get_rank();
      size = MPI::COMM_WORLD.Get_size();

      char *b = buffer;
      MPI::Get_processor_name(b, len);
      processor = buffer;

      std::ostringstream out;

      out.str("");
      out << Cluster::rank;
      rank_str = out.str();

      out.str("");
      out << Cluster::size;
      size_str = out.str();
    };

    void finalize(void){
      MPI::Finalize();
    };

  }
}
