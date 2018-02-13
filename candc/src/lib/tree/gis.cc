// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

///////////////////////////////////////////////////////////////////////////
//
// This file is part of the C&C NLP software package.
//   
// C&C is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2 of the License, or (at your
// option) any later version.
//
// C&C is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with C&C; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
///////////////////////////////////////////////////////////////////////////

#include <mpi.h>

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <sstream>

using namespace std;

#include "port.h"

#include "utils.h"
#include "except.h"

#include "input.h"

#include "timer.h"

#include "tree/options.h"
#include "tree/feature.h"
#include "tree/node.h"
#include "tree/forest.h"
#include "tree/gis.h"

#include "cluster.h"

#include <sys/types.h>
#include <unistd.h>

namespace NLP { namespace Tree {

GIS::GIS(string filename, bool load_forests): op(filename) {
  _read_features();

  if(load_forests){
    _read_forests();

    // SC: added this - calculates the empirical feature counts
    _calc_emp();
  }

  // turned off for BFGS testing (speeds things up a bit)
  //  cout << Cluster::rank << " calculating correction constant maxC" << endl;
  //_calc_correction();
  C = 0.0;
}

GIS::~GIS(void) {
}

// SC: added this function
void GIS::_calc_emp(){
  // this updates the local feature emp counts
  //for(Forests::iterator forest = forests.begin(); forest != forests.end(); ++forest)
  //(*forest)->calc_emp(features);

    //    if(!(*forest)->calc_emp())
    //cerr << "not finding a correct derivation for the emp counts\n";

  //sc: (23/9/03) we're doing this differently now 

  if(op.norm_form){
    //sc: okay to use local_est to store local emp counts
    for(ulong i = 0; i < features.size(); ++i)
      local_est[i] = features[i].emp;

    //  ostringstream out;
    //out << "calculated local emp on " << Cluster::processor << " pid " << getpid() << '\n';
    //cerr << out.str();

    memset(global_est, 0, sizeof(double)*op.nfeatures);
    MPI::COMM_WORLD.Allreduce(local_est, global_est, op.nfeatures, MPI::DOUBLE, MPI::SUM);

    for(ulong i = 0; i < features.size(); ++i)
      features[i].emp = global_est[i];

    //  out.str("");
    //out << "finished calc_emp() " << Cluster::rank << ' ';
    //out << Cluster::processor << " pid " << getpid() << '\n';
    //cerr << out.str();
  }
  //sc: this function just marks all the correct derivations
  else{
    for(Forests::iterator forest = forests.begin(); forest != forests.end(); ++forest)
      (*forest)->mark_correct();
  }
}

void GIS::_read_features(void) {
  ulong nlines = 0;

  tfeatures = 0;
  features.reserve(op.nfeatures);
  local_est = new double[op.nfeatures];
  global_est = new double[op.nfeatures];

  const string filename = op.model + "/features";
  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open features file", filename);

  comment += read_preface(filename, in, nlines);

  ulong freq;
  while(in >> freq){
    //sc: passing in zero freq - we never use the precomputed figure
    features.push_back(Feature(0.0));
    tfeatures += freq;
    in.ignore(1024, '\n');
  }

  if(!in.eof())
    throw NLP::IOException("error reading in feature", filename);

  if(features.size() != op.nfeatures)
    throw NLP::IOException("number of read features does not match config file", filename);
}

void GIS::_read_forests(void) {
  ulong nlines = 0;
  forests.reserve(op.nsentences);
  nforests = 0;
  long local_nforests = 0;
  long local_nread = 0;
  long local_nreject = 0;

  const string filename = op.base + '.' + Cluster::rank_str + ".out";
  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open forests file for reading", filename);

  comment += read_preface(filename, in, nlines);

  //SC: change here - only add forest if there is at least 1 correct derivation
  ulong sentence;
  while(in >> sentence){
    Forest *forest = new Forest(in, features);
    if(op.norm_form){
      local_nread++;
      if(forest->check_deriv(features)){
	forests.push_back(forest);
	local_nforests++;
	forest->feat_counts();
      }else{
	delete forest;
	local_nreject++;
      }
    }else{
      local_nread++;
      double correct_derivs = forest->count_correct();
      if(correct_derivs != -1.0){
	forests.push_back(forest);
	local_nforests++;
      }else{
	delete forest;
	local_nreject++;
      }
    }
  }
  if(!in.eof())
    throw NLP::IOException("error reading in forest", filename);

  ostringstream out;
  out << "calculated local forest count: " << local_nforests
      << " on " << Cluster::processor << " pid " << getpid() << '\n';
  cerr << out.str();  

  MPI::COMM_WORLD.Allreduce(&local_nforests, &nforests, 1, MPI::LONG, MPI::SUM);
  MPI::COMM_WORLD.Allreduce(&local_nreject, &nreject, 1, MPI::LONG, MPI::SUM);
  MPI::COMM_WORLD.Allreduce(&local_nread, &nread, 1, MPI::LONG, MPI::SUM);
  if(Cluster::rank == 0){
    out.str("");
    out << "total number of forests: " << nforests << endl;
    out << "total number of nreject: " << nreject << endl;
    out << "total number of nread: " << nread << endl;
    cerr << out.str();
  }
}

void GIS::_calc_correction(void) {
  ulong max_active = 0;
  for(Forests::iterator forest = forests.begin(); forest != forests.end(); ++forest)
    max_active = max(max_active, (*forest)->max_active());

  ulong global_max_active = 0;

  MPI::COMM_WORLD.Allreduce(&max_active, &global_max_active, 1, MPI::UNSIGNED_LONG, MPI::MAX);

  C = global_max_active;
  invC = 1.0/static_cast<double>(C);
}

void GIS::profile(void) {
  cout << "nfeatures = " << op.nfeatures << endl;
  cout << "tfeatures = " << tfeatures << endl;

  cout << "C = " << C << endl;

  cout << "smoothing = " << Options::Smoothing::to_string(op.smoothing) << endl;
  if(op.gaussian())
    cout << "alpha = " << op.alpha << endl;

  cout << "-----------------------------------\n";
  cout << "feature memory = " << op.nfeatures*sizeof(Feature) << endl;
}

//SC: introduced this function

void GIS::_count_correct(void){
  ulong forest_num = 1;
  for(Forests::iterator forest = forests.begin(); forest != forests.end(); ++forest){
    (*forest)->count_correct();
    //    cout << "forest_num: " << forest_num << endl;
    //cout << "log number of correct parses: " << num_correct << endl;
    forest_num++;
    //cout << endl;
  }
}

void GIS::_reduce_counts(void){
  for(Forests::iterator forest = forests.begin(); forest != forests.end(); ++forest)
    (*forest)->count_features();
  for(ulong i = 0; i < features.size(); ++i)
    local_est[i] = features[i].est;

  memset(global_est, 0, sizeof(double)*op.nfeatures);
  MPI::COMM_WORLD.Allreduce(local_est, global_est, op.nfeatures, MPI::DOUBLE, MPI::SUM);

  for(ulong i = 0;  i < op.nfeatures; ++i)
    if(features[i].emp > global_est[i])
      features[i].emp = global_est[i];
}

void GIS::iterate(void) {
  //  _count_correct();
  //exit(0);

  //_reduce_counts();

  for(ulong i = 0; i < op.nfeatures; ++i)
    features[i].est = 0.0;

  for(niterations = 0; niterations < op.niterations; ++niterations){
    if(Cluster::rank == 0)
      cout << "iteration: " << niterations << flush;

    if(Cluster::rank == 0)
      cout << ", distribute" << flush;

    double llhood = 0.0;
    for(ulong i = 0; i < forests.size(); ++i){
      double Z = forests[i]->inside(op.norm_form);
      forests[i]->outside(-Z, op.norm_form);
      if(op.norm_form)
	llhood += forests[i]->llhood(true);
      else
	llhood += forests[i]->llhood(false);
    }

    double total_llhood = 0.0;
    MPI::COMM_WORLD.Allreduce(&llhood, &total_llhood, 1, MPI::DOUBLE, MPI::SUM);
    if(Cluster::rank == 0)
      cout << ", llhood = " << total_llhood;

    if(Cluster::rank == 0)
      cout << ", reduce" << flush;

    for(ulong i = 0; i < op.nfeatures; ++i)
      local_est[i] = features[i].est;
    memset(global_est, 0, op.nfeatures*sizeof(double));
    MPI::COMM_WORLD.Allreduce(local_est, global_est, op.nfeatures, MPI::DOUBLE, MPI::SUM);

    if(Cluster::rank == 0)
      cout << ", updating" << flush;

    switch(op.smoothing){
      case Options::Smoothing::NONE:
        _update();
        break;
      case Options::Smoothing::GAUSSIAN:
        _update_gaussian();
        break;
      default: assert(!"unexpected smoothing value");
    }
    if(Cluster::rank == 0){
      if(niterations % 1 == 0){
	cout << ", saving" << flush;
	save(niterations);
      }

      cout << ", done" << endl;
    }
  }
}

void GIS::_normalise(double invZ){
}

void GIS::_update(void){
  if(Cluster::rank == 0){
    double total_weight = 0;
    double total_emp = 0;
    double total_est = 0;
    for(ulong i = 0;  i < features.size(); ++i){
      features[i].est = global_est[i];
      total_emp += features[i].emp;
      total_est += global_est[i];
      features[i].update(invC);
      total_weight += features[i].lambda;
    }
    cout << ' ' << total_emp << ' ' << total_est << ' ' << total_weight;
  }else{
    for(ulong i = 0;  i < features.size(); ++i){
      features[i].est = global_est[i];
      features[i].update(invC);
    }
  }
}

void GIS::_update_gaussian(void){
  if(Cluster::rank == 0){
    double total_weight = 0;
    double total_emp = 0;
    double total_est = 0;
    for(ulong i = 0;  i < features.size(); ++i){
      features[i].est = global_est[i];
      total_emp += features[i].emp;
      total_est += global_est[i];
      features[i].update_gaussian(C, op.alpha);
      total_weight += features[i].lambda;
    }
    cout << ' ' << total_emp << ' ' << total_est << ' ' << total_weight;
  }else{
    for(ulong i = 0;  i < features.size(); ++i){
      features[i].est = global_est[i];
      features[i].update_gaussian(C, op.alpha);
    }
  }
}

void GIS::save(void) {
  const string filename = op.model + "/weights";
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open weights file for writing", filename);

  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    out << feature->lambda << '\n';
}

void GIS::save(long iteration) {
  ostringstream s;

  s << op.model << "/expect" << iteration;

  string filename = s.str();
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open weights file for writing", filename);

  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    out << feature->lambda << " emp: " << feature->emp << " est: " << feature->est << '\n';
}

void
GIS::stats(void){
  double usage = Port::get_usage()/1024.0*1024.0;
  double total_usage = 0.0;

  MPI::COMM_WORLD.Allreduce(&usage, &total_usage, 1, MPI::DOUBLE, MPI::SUM);
  if(Cluster::rank == 0){
    const string filename = op.model + "/stats";
    ofstream out(filename.c_str());
    if(!out)
      throw NLP::IOException("could not open weights file for writing", filename);

    out << "number of iterations = " << niterations << endl;
    out << "memory usage = " << total_usage << endl;
    out << "CPU time = " << watch.stop() << endl;
  }
}

} }
