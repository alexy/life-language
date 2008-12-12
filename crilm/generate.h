#ifndef __generate_h__
#define __generate_h__

#include "LM.h"
#include "Vocab.h"

VocabIndex *
generateSentence(const LM* lm, unsigned maxWords, const VocabIndex*  sentence, const unsigned senlen, bool allow_se = false);

VocabString *
generateSentence(const LM* lm, unsigned maxWords, const VocabString* sentence, const unsigned senlen, bool allow_se = false);

#endif