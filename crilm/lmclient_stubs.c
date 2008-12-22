/* OCaml bindings for SRILM
   Copyright (c) Alexy Khrabrov <deliverable@gmail.com>, 2008.
   Released under GPLv3 -- see FSF.org for details */
   
#include "LM.h"
#include "LMClient.h"
#include "File.h"
#include "Vocab.h"

#include "generate.h"

#include <sstream>
#include <tcl.h>

extern "C" {
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/signals.h>
}


#define MAX_LM_CLIENTS 100
// NB we can replace this static array by STL vector
static LMClient *lmclient[MAX_LM_CLIENTS] = {0};
static int num_clients = 0;
static bool valid_handle(int lm_handle) {
  return num_clients >= 0 && lm_handle >= 0 
    && lm_handle <= num_clients 
    && lmclient[lm_handle-1] != 0;
}


// NB can we have lmclient_null take void, 
// without even a dummy value for unit?
extern "C" value lmclient_null (value v_unit) {
  CAMLparam1 (v_unit);
  CAMLreturn (Val_int(0));
}

extern "C" value lmclient_is_null (value v_server) {
  CAMLparam1 (v_server);
  CAMLreturn (Val_bool(Int_val(v_server) == 0));
}

// NB here we do nothing but receive-return, macros do the conversion
// from the abstract type to int!  yay!  Before that was figured out,
// the extraneous Val_int() in CAMLreturn returned mysterious 3 and 5
// instead of the correct 1 and 2 in caml.ml
extern "C" value lmclient_int_of_handle (value v_server) {
  CAMLparam1 (v_server);
  // cerr << "C++ int_of_handle is about to return: " << Int_val(v_server) << endl;
  CAMLreturn (v_server);
}


extern "C" value lmclient_complete_sentence (value v_lm_handle, value v_maxwords, value v_sarray) {
  CAMLparam3(v_lm_handle,v_maxwords,v_sarray);
  CAMLlocal1 (result);
  
  int maxwords   = Int_val(v_maxwords);
  int prefix_length = Wosize_val(v_sarray);
  // cerr << "C++ got array of size " << array_size << " with elements:" << endl;
  // cerr << "they've asked us to generate a sentence of length => " << maxwords << endl;

  int lm_handle = Int_val (v_lm_handle);
  if (!valid_handle(lm_handle)) {
    CAMLreturn (caml_copy_string(""));
  }
  int lm_index = lm_handle - 1;
  LMClient *useLM = ::lmclient[lm_index];

  if (maxwords <= prefix_length) maxwords = prefix_length + 1;
   
  const unsigned sentence_length = maxwords + 1;

  VocabString *sentence = new VocabString[sentence_length];
  assert(sentence != 0);
  for (int i = 0; i < prefix_length; ++i) {
    sentence[i] = String_val(Field(v_sarray, i));
  }
  sentence[prefix_length] = NULL;   
  
  useLM->generateSentence(maxwords-prefix_length+1, &sentence[prefix_length], sentence);

#ifdef DEBUG
  cerr << "we have a sentence!" << endl;
#endif
  result = caml_alloc(maxwords, 0);
  for (int i = 0; i < maxwords && sentence[0] != NULL; ++i) {
#ifdef DEBUG
    cerr << i << ": " << sentence[i] << endl;
#endif
    Store_field(result, i, caml_copy_string(sentence[i]));
  }
  
  delete sentence; // should be auto upon out-of-scope, but just in case
  CAMLreturn (result);
}



extern "C" value lmclient_create (value v_server, value v_order, value v_vocab) {
  CAMLparam3 (v_server, v_order, v_vocab);
  const char *vocab_filename = String_val(v_vocab);

  // NB: flush somehow gets translated into caml_flush, which is not available,
  // even with include <caml/compatibility.h> where it's defined?!
  #define _flush flush
  #undef flush
  // cerr << "vocab_filename: [" << vocab_filename << "]" << endl << flush;
  
  if (num_clients >= MAX_LM_CLIENTS) {
    CAMLreturn (Val_int(-1));
  }
  
  LMClient *useLM;
  LM::initialDebugLevel = 1;
  
  Vocab *vocab = new Vocab;
  assert(vocab != 0);

  if (strlen(vocab_filename) > 0) {
      cerr << "client " << (num_clients+1) << " reading vocab " << vocab_filename << endl << flush;
      File vocab_file(vocab_filename, "r"); // vocab_file.name == vocab_filename
      vocab->read(vocab_file);
      }

  vocab->remove(vocab->ssIndex());
  vocab->remove(vocab->seIndex());
  vocab->remove(vocab->pauseIndex());
  
  #define flush _flush
  
  useLM = new LMClient(
    /*Vocab &*/ *vocab, 
    /*const char */ String_val (v_server), 
    /* unsigned*/ Int_val (v_order), /*unsigned cacheOrder*/ 0);

  // handles are always positive, 1-based
  ::lmclient[num_clients++] = useLM;
  // cerr << "C++ returns a new handle: " << num_clients << endl;
  CAMLreturn (Val_int(num_clients));
}

extern "C" value lmclient_destroy (value v_lm_handle) {
  CAMLparam1 (v_lm_handle);
  
  int lm_handle = Int_val(v_lm_handle);
  
  if (!valid_handle(lm_handle)) {
    CAMLreturn (Val_int(-1));
  }
  int lm_index = lm_handle - 1;
  
  delete ::lmclient[lm_index]; // reuse would be handy...
  ::lmclient[lm_index] = 0;
  
  // return the number of clients remaining
  CAMLreturn (Val_int(--num_clients)); 
}

extern "C" value lmclient_compute (value v_lm_handle, value v_filename) {
  // don't really know whether we need this, trying everything
  // against deadlock -- was not that, so commenting out a critical section:
  // enter_blocking_section();
  
  CAMLparam2 (v_lm_handle, v_filename);

  int lm_handle = Int_val (v_lm_handle);
  //cerr << "compute got lm_handle=" << lm_handle << endl;

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
  { cerr << "C++ received empty filename for ppl" << endl; exit(1); } // complain!
	
	File file(String_val(v_filename), "r");
	TextStats stats;

  ostringstream oss;
  
	/*
	 * Send perplexity info to stdout 
	 */
	useLM->dout(oss); // instead of cout!
	useLM->pplFile(file, stats, /* escape */ 0);
	useLM->dout(cerr);

  // only need stats if it's for a single sentence file:
	// cerr << "file " << pplFile << ": " << stats;
  // oss << stats; 
  
  string str = oss.str();  
  
  // leave_blocking_section();
  CAMLreturn (caml_copy_string(str.c_str()));
}

/*
extern "C" int Tcl_AppInit(Tcl_Interp *interp)
{
  return 0;
}
*/

