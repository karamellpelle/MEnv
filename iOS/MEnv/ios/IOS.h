/*
 *   Copyright (c) 2013, Carl Joachim Svenn
 *   All rights reserved.
 *   
 *   Redistribution and use in source and binary forms, with or without 
 *   modification, are permitted provided that the following conditions are met:
 *   
 *       1. Redistributions of source code must retain the above copyright notice, this 
 *          list of conditions and the following disclaimer.
 *       2. Redistributions in binary form must reproduce the above copyright notice, 
 *          this list of conditions and the following disclaimer in the documentation 
 *          and/or other materials provided with the distribution.
 *   
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 *   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
 *   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
 *   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 *   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
 *   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
 *   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#import <OpenGLES/ES2/gl.h>
#import <QuartzCore/QuartzCore.h>
#import "IOSScreen.h"
#import "IOSKeys.h"
#import "IOSTick.h"
#import "debug.h"

// assert:
//   alignment = 4
//   sizeof = 4 + 4 + 4
//   sizeof screen_multisample = 4, offset = 0
//   sizeof screen_orientations = 4,  offset = 4
//   sizeof sound_rate = 4,  offset = 8
//   sizeof keys_acclgyro_rate = 4, offset = 12
typedef struct
{
    uint screen_multisample;
    uint screen_orientations;
    uint screen_rate;
    uint sound_rate;
    float keys_acclgyro_rate;
    
} IOSInit;


extern IOSInit theIOSInit;


// type of callback into Haskell
typedef void (*HaskellCall)();

extern HaskellCall haskell_begin;

extern HaskellCall haskell_iterate;



void ios_init(IOSInit* );

void ios_main(HaskellCall , HaskellCall );

uint ios_fileStaticData(const char* , char* , uint );

uint ios_fileDynamicData(const char* , char* , uint );

uint ios_fileUser(const char* , char* , uint );
