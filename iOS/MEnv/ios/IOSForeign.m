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
#import "IOSForeign.h"
#import "IOS.h"
#import "IOSAppDelegate.h"
#import "IOSViewController.h"
#import <UIKit/UIKit.h>




// locals
static IOSForeignStateT ios_foreign_state;

static IOSForeignStateT ios_foreign_state_next;




void ios_foreignBeginForeign()
{
    ios_foreign_state_next = ForeignStateForeign;

    [theIOSAppDelegate setHaskellIterate: (NO)];
    [theIOSAppDelegate setViewController: theForeignViewController];
    ios_foreign_state = ios_foreign_state_next;

}


// do not call this function from Haskell!
void ios_foreignEndForeign()
{
    // prevent call from foreignBeginForeign
    if ( ios_foreign_state == ForeignStateForeign )
    {
        ios_foreign_state_next = ForeignStateHaskell;
        [theIOSAppDelegate setViewController: theIOSViewController];
        haskell_iterate();
        ios_foreign_state = ios_foreign_state_next;
        [theIOSAppDelegate setHaskellIterate: (YES)];
    }
}


uint ios_foreignHandleForeignEnd()
{
    return  ios_foreign_state == ForeignStateForeign &&
            ios_foreign_state_next == ForeignStateHaskell;
        
}
