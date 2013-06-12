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
#import "IOSAppDelegate.h"
#import "IOSViewController.h"
#import "IOSView.h"
#import "IOSSystem.h"
#import "IOSForeign.h"
#import "IOSPlayers.h"


// globals
IOSAppDelegate* theIOSAppDelegate = nil;



@implementation IOSAppDelegate

@synthesize window = window_;

@synthesize rootViewController = rootViewController_;



- (void)dealloc
{   
    foreign_destroy();
    
    [rootViewController_ release];
    [window_ release];
    // [theEAGLContext release]; ??
    [super dealloc];
}


// start from UIApplication
// (launchOptions contains program information (user launch, URL launch))
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{

    NSLog(@"IOSAppDelegate: didFinishLaunchingWithOptions");
        
    // set the canonical IOSAppDelegate
    theIOSAppDelegate = self;


    // init Keys
    ios_initKeys();
    
    // init Screen
    ios_initScreen();
    
    // init Sound
    ios_initSound();
    
    // init Tick
    ios_initTick();
    
    // init Players
    ios_initPlayers();
    
    
    // create window
    window_ = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    [[UIApplication sharedApplication] setStatusBarHidden:(YES) withAnimation:UIStatusBarAnimationSlide];

    // make default ViewController (iteration of Haskell)
    [[IOSViewController alloc] init];
    [self setViewController: theIOSViewController];

    // make foreign ViewController (iteration of Objective-C)
    foreign_make();
    
    return YES;
}


- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
    
    // end active
    NSLog(@"IOSAppDelegate: front end");

    ios_system_state_next = SystemStateMiddle;
    haskell_iterate();
    ios_system_state = ios_system_state_next;
        

}


- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    
    // begin background
    NSLog(@"IOSAppDelegate: back begin");
    
    ios_system_state_next = SystemStateBack;
    haskell_iterate();
    ios_system_state = ios_system_state_next;
    
    // stop GL (GL commands not allowed in background state)
    // fixme: release GL framebuffers and children to save memory!
    glFinish();
    
    [self setHaskellIterate:(NO)];

}


// end Back state
- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
    
    // end background
    NSLog(@"IOSAppDelegate: back end");
    
    ios_system_state_next = SystemStateMiddle;
    haskell_iterate();
    ios_system_state = ios_system_state_next;
    
    [self setHaskellIterate:(YES)];
}


// begin Front state
- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    // begin active
    NSLog(@"IOSAppDelegate: front begin");
    
    [window_ makeKeyAndVisible];
    
    // if first time, begin in Front-state
    if ( ios_system_state == SystemStateEmpty )
    {
        ios_system_state = SystemStateFront;
        ios_system_state_next = SystemStateFront;
        
        // set window
        [window_ makeKeyAndVisible];
        
        // begin haskell
        haskell_begin();
        
        [self setHaskellIterate:YES];
        
    }
    else
    {        
        ios_system_state_next = SystemStateFront;
        haskell_iterate();
        ios_system_state = ios_system_state_next;
    }

    
}


- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    NSLog(@"IOSAppDelegate: terminating");
    
    [self setHaskellIterate:NO];
    
    ios_system_state_next = SystemStateEmpty;
    haskell_iterate();
    ios_system_state = ios_system_state_next;
    
}


// fixme: release memory in Haskell?
-(void)applicationDidReceiveMemoryWarning:(UIApplication *)application
{
    
}


-(void)setViewController:(UIViewController *)vc
{
    [window_ setRootViewController:vc];
}


-(void)setHaskellIterate:(BOOL)value
{
    [displaylink_ setPaused: (!value)];
}


-(void)setHaskellRate:(uint)rate
{
    // frames_per_sec = 60 frames_per_sec / r
    // 1        -> 60
    // 2        -> 30
    // 12       -> 5

    [displaylink_ setFrameInterval: rate];

}

@end
