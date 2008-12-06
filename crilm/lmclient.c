#include "LM.h"
#include "LMClient.h"
#include "File.h"

#include <sstream>

extern "C" {
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
}

#define MAX_LM_CLIENTS 100
static LMClient *lmclient[MAX_LM_CLIENTS] = {0};
static int num_clients = 0;
static bool valid_handle(int lm_handle) {
  return num_clients >= 0 && lm_handle >= 0 
    && lm_handle <= num_clients 
    && lmclient[lm_handle-1] != 0;
}

extern "C" value lmclient_create (value v_server, value v_order) {
  CAMLparam2 (v_server, v_order);
  
  if (num_clients >= MAX_LM_CLIENTS) {
    CAMLreturn (caml_copy_nativeint(-1));
  }
  
  LMClient *useLM;
  
  Vocab *vocab = new Vocab;
  
  useLM = new LMClient(
    /*Vocab &*/ *vocab, 
    /*const char */ String_val (v_server), 
    /* unsigned*/ Int_val (v_order), /*unsigned cacheOrder*/ 0);

  // handles are always positive, 1-based
  ::lmclient[num_clients++] = useLM;
  CAMLreturn (Val_int(num_clients));
}

extern "C" value lmclient_destroy (value v_lm_handle) {
  CAMLparam1 (v_lm_handle);
  
  int lm_handle = Int_val(v_lm_handle);
  
  if (!valid_handle(lm_handle)) {
    CAMLreturn (Int_val(-1));
  }
  int lm_index = lm_handle - 1;
  
  delete ::lmclient[lm_index]; // reuse would be handy...
  ::lmclient[lm_index] = 0;
  
  // return the number of clients remaining
  CAMLreturn (Val_int(--num_clients)); 
}

extern "C" value lmclient_compute (value v_lm_handle, value v_filename) {
  CAMLparam2 (v_lm_handle, v_filename);

  int lm_handle = Int_val (v_lm_handle);
  cout << "compute got lm_handle=" << lm_handle << endl;

  if (!valid_handle(lm_handle)) {
    CAMLreturn (caml_copy_string(""));
  }

  int lm_index = lm_handle - 1;
  LMClient *useLM = ::lmclient[lm_index];
  
// from ngram.cc
  /*
   * Compute perplexity on a text file, if requested
   */
  if (!String_val(v_filename)) 
  { cout << "C++ received empty filename for ppl" << endl; exit(1); } // complain!
	
	File file(String_val(v_filename), "r");
	TextStats stats;

	/*
	 * Send perplexity info to stdout 
	 */
	useLM->dout(cout);
	useLM->pplFile(file, stats, /* escape */ 0);
	useLM->dout(cerr);

	// cout << "file " << pplFile << ": " << stats;
  ostringstream oss;
  oss << stats; 
  string str = oss.str();  
  
  CAMLreturn (caml_copy_string(str.c_str()));
}
