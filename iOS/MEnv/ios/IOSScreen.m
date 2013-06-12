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
#import "IOSScreen.h"
#import "IOSView.h"
#import "IOSAppDelegate.h"


EAGLContext* theEAGLContext = nil;

const uint ios_screen_rate_default = 1;



BOOL ios_shouldAutorotateToInterfaceOrientation(UIInterfaceOrientation question)
{
    uint bits = theIOSInit.screen_orientations;
    
    switch (question)
    {
        case UIInterfaceOrientationPortrait:
            return ( bits & IOS_ORIENTATION_PORTRAIT ) != 0;
            
        case UIInterfaceOrientationPortraitUpsideDown:
            return ( bits & IOS_ORIENTATION_PORTRAIT_FLIPPED ) != 0;
            
        case UIInterfaceOrientationLandscapeLeft:
            return ( bits & IOS_ORIENTATION_LANDSCAPE_LEFT ) != 0;
            
        case UIInterfaceOrientationLandscapeRight:
            return ( bits & IOS_ORIENTATION_LANDSCAPE_RIGHT ) != 0;
            
    }
    return NO;
}


void ios_initScreen()
{

    // create OpenGL ES 2.0 context for this thread
    theEAGLContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    if ( !theEAGLContext || ![EAGLContext setCurrentContext:theEAGLContext] )
    {
        NSLog(@"ios_initScreen: could not create OpenGL context!");
    }

    // screen_rate == 0 is special, but handled by ios_screenSetRate
    ios_screenSetRate( theIOSInit.screen_rate );

    // force OpenGL/IOSView FBO to be present!
    [[IOSView alloc] init];
    
    
    
}


void ios_screenSize(uint* wth, uint* hth)
{
    *wth = (uint) theIOSView->wth_;
    *hth = (uint) theIOSView->hth_;
}


uint ios_screenFBO()
{
    return theIOSView->multisample_framebuffer_;
    
}


void ios_screenSetRate(uint rate)
{
    printf("screenSetRate %u\n", rate);
    
    // if 0, reset screen rate to value defined by 'theIOSInit'
    uint r;
    if (rate == 0)
    {
        r = theIOSInit.screen_rate == 0 ? ios_screen_rate_default : theIOSInit.screen_rate;
    }
    else
    {
        r = rate;
    }
    [theIOSAppDelegate setHaskellRate:r];

}

