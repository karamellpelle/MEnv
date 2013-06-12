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
#import "IOS.h"
#import "IOSSound.h"
#import <AudioToolbox/AudioToolbox.h>
#import <OpenAL/al.h>
#import <OpenAL/alc.h>


ALCcontext* theALCcontext = 0;
ALCdevice* theALCdevice = 0;







static void ios_interrupt_f(void* data, UInt32 state)
{
    switch (state)
    {
        // begin interruption
        case kAudioSessionBeginInterruption:
            alcSuspendContext( theALCcontext );
            alcMakeContextCurrent( 0 );
            
        break;
        
        // end interruption
        case kAudioSessionEndInterruption:
            if ( AudioSessionSetActive( true ) ) 
            {
                printf("ios_interrupt_f: could not activate AudioSession!\n");
            }
            alcMakeContextCurrent( theALCcontext );
            alcProcessContext( theALCcontext );
            
        break;
            
    }
}


// set up Sound.
// see documentation "Audio Session Cookbook"
// fixme: use settings from theIOSSoundInit
void ios_initSound()
{
    printf("ios_initSound\n");
    // init AudioSession 
		if ( AudioSessionInitialize(NULL, NULL, ios_interrupt_f, 0) )
    {
        printf("ios_initSound: could not initialize AudioSession!\n");
    }
    
    // set category for AudioSession
    UInt32 cat = kAudioSessionCategory_SoloAmbientSound;
    AudioSessionSetProperty( kAudioSessionProperty_AudioCategory, sizeof(UInt32), &cat );

    // activate AudioSession
    if ( AudioSessionSetActive( true ) )
    {
        printf("ios_initSound: could not activate AudioSession!\n");
    }
    
    // init OpenAL
    theALCdevice = alcOpenDevice( 0 );
    theALCcontext = alcCreateContext( theALCdevice, 0 );
    alcMakeContextCurrent( theALCcontext );
    
    // fixme: set mixer rate.
    // alcMacOSXMixerOutputRate ?
    // kAudioSessionProperty_PreferredHardwareSampleRate ?
    typedef ALvoid AL_APIENTRY (*alcMacOSXMixerOutputRateProcPtr)
                               (const ALdouble value);
    alcMacOSXMixerOutputRateProcPtr proc_ptr = 
            alcGetProcAddress(NULL, (const ALCchar*) "alcMacOSXMixerOutputRate");
    if ( proc_ptr )
    {
        proc_ptr( (ALdouble)theIOSInit.sound_rate );
    }


}



