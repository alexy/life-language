#include "generate.h"
#include <iostream>

VocabIndex *
generateSentence(const LM* lm, unsigned maxWords, const VocabIndex* sentence, const unsigned conlen, bool allow_se)
{

  if (maxWords <= conlen) maxWords = conlen + 1; // generate at least one word!

  // cerr << "context indices, total " << conlen << endl;
  // for (int i = 0; i < conlen; ++i) cerr << i << ": " << sentence[i] << endl;
  
  /*
   * Since we need to add the begin/end sentences tokens, and
   * partial contexts are represented in reverse we use a second
   * buffer for partial sentences.
   */
   
  makeArray(VocabIndex, genBuffer, maxWords + 2); // was 3 for suffix <s>;Vocab_None

  unsigned last = maxWords + 1; // was 2 for -"-
  genBuffer[last] = Vocab_None;
  // we don't have <s> in sensor data per se
  // genBuffer[--last] = vocab.ssIndex();

  for (int i = 0; i < conlen && sentence[i] != Vocab_None && last > 0; ++i) {
    --last;
    genBuffer[last] = sentence[i];
    // cerr << "position " << last << " copied " << genBuffer[last] << endl;
  }
  /*
   * Generate words one-by-one until hitting an end-of-sentence.
   */
   
  // actually using </s>, which we don't have in data, below
  // && genBuffer[last] != lm->vocab.seIndex() 
  // lead to early stop, garbage in indices, and segfault in 
  // getWords(indices)=>print words (they were garbage in turn)
  
  VocabIndex word;
  const VocabIndex endS = lm->vocab.seIndex();

  while (last > 0 /* && genBuffer[last] != lm->vocab.seIndex() */) {
    --last;
    // cerr << "position " << last;
    do {
      // in generateWord's this is not const LM, but plain:
      word = ((LM*)lm)->generateWord(&genBuffer[last + 1]);
    } while (!allow_se && word == endS);
    genBuffer[last] = word;
    // cerr << " generated " << genBuffer[last] << endl;
  }
    
  /*
   * Copy reversed sentence to output buffer
   */
   
  VocabIndex *result = new VocabIndex[maxWords];
  
  unsigned i, j;
  for (i = 0, j = maxWords; j > last; i++, j--) {
    result[i] = genBuffer[j];
  }
  result[i] = Vocab_None;

  // cerr << "generated indices, total " << maxWords << endl;
  // for (i = 0; i <= maxWords; ++i) cerr << i << ": " << result[i] << endl;
  return result;
}


VocabString *
generateSentence(const LM* lm, unsigned maxWords, const VocabString* sentence, const unsigned conlen, bool allow_se)
{
  // convert initial context to indices
  
  VocabString *result = new VocabString[maxWords]; // +1?
    
  /*
   * Generate words indices, then map them to strings
   */
     
  makeArray(VocabIndex,indices,conlen+1);
     
  lm->vocab.getIndices(sentence, indices, conlen);
  indices[conlen] = Vocab_None;
  
  lm->vocab.getWords(generateSentence(lm, maxWords, indices, conlen, allow_se),
	                   result, maxWords); // +1?

  return result;
}
