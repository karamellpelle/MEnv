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
#import "IOSView.h"
#import "IOSScreen.h"
#import "IOSKeys.h"
#import "IOSAppDelegate.h"
#import <QuartzCore/QuartzCore.h>



// globals
IOSView* theIOSView = nil;


@implementation IOSView


-(void) dealloc
{
    [self destroyFramebuffers];
        
    [super dealloc];
}


-(id) init
{
    NSLog(@"IOSView: init");

    CGRect screenFrame = [[UIScreen mainScreen] bounds];
    self = [super initWithFrame:screenFrame ];
    
    if ( self )
    {
        // set layer
        layer_ = (CAEAGLLayer*) self.layer;
        layer_.opaque = YES;
        
        // set event handling
        [self setMultipleTouchEnabled:YES];
        
        // set the canonical IOSView
        theIOSView = self;
        
        // setup iteration of this view //
        
        // 
        CADisplayLink* displaylink = theIOSAppDelegate->displaylink_;
        bool paused;
        bool interval;        
        if (displaylink == nil)
        {
            paused = true;
            interval = theIOSInit.screen_rate == 0 ? ios_screen_rate_default :
                                                     theIOSInit.screen_rate;
        }
        else
        {
            paused = displaylink.paused;
            interval = displaylink.frameInterval;
        }

        
        // remove old
        [displaylink setPaused:(YES)];
        [displaylink invalidate];
        
        displaylink = [CADisplayLink displayLinkWithTarget:self
                                     selector:@selector(iterateIOSView)];
        
        [displaylink setPaused: paused];
        [displaylink setFrameInterval: interval];
        [displaylink addToRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        
        theIOSAppDelegate->displaylink_ = displaylink;
        
        // set up FBO
        [self createFramebuffers];
        
    }
    
    return self;
}


- (void)layoutSubviews {
    NSLog(@"IOSView: layoutSubviews");
    
    //[EAGLContext setCurrentContext:theEAGLContext];
    [self destroyFramebuffers];
    [self createFramebuffers];

}


-(void) createFramebuffers
{
    // resolve framebuffer
    glGenFramebuffers(1, &resolve_framebuffer_);
    glBindFramebuffer(GL_FRAMEBUFFER, resolve_framebuffer_);
    
    glGenRenderbuffers(1, &resolve_renderbuffer_);
    glBindRenderbuffer(GL_RENDERBUFFER, resolve_renderbuffer_);
    [theEAGLContext renderbufferStorage:GL_RENDERBUFFER fromDrawable:layer_];
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, resolve_renderbuffer_);
    
    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_WIDTH, &wth_);
    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_HEIGHT, &hth_);
    
    if(glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
    {
        printf("IOSView: failed to make resolve framebuffer\n");
    }
    
    
    // multisampling framebuffer
    
    const GLsizei multisampling_number = theIOSInit.screen_multisample;
    
    glGenFramebuffers(1, &multisample_framebuffer_);
    glBindFramebuffer(GL_FRAMEBUFFER, multisample_framebuffer_);
    
    glGenRenderbuffers(1, &multisample_renderbuffer_);
    glBindRenderbuffer(GL_RENDERBUFFER, multisample_renderbuffer_);
    glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER, multisampling_number, GL_RGBA8_OES, wth_, hth_);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, multisample_renderbuffer_);
    
    // fixme: remove stencil?
    glGenRenderbuffers(1, &multisample_depthstencilbuffer_);
    glBindRenderbuffer(GL_RENDERBUFFER, multisample_depthstencilbuffer_);
    glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER, multisampling_number, GL_DEPTH24_STENCIL8_OES, wth_, hth_);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, multisample_depthstencilbuffer_);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, multisample_depthstencilbuffer_);
    
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
    {
        printf("IOSView: failed to make multisample framebuffer\n");
    }

}


-(void) destroyFramebuffers
{
    glDeleteRenderbuffers(1, &resolve_renderbuffer_);
    glDeleteFramebuffers(1, &resolve_framebuffer_);
    
    glDeleteRenderbuffers(1, &multisample_depthstencilbuffer_);
    glDeleteRenderbuffers(1, &multisample_renderbuffer_);
    glDeleteFramebuffers(1, &multisample_framebuffer_);

    resolve_framebuffer_ = 0;
    resolve_renderbuffer_ = 0;
    multisample_depthstencilbuffer_ = 0;
    multisample_renderbuffer_ = 0;
    multisample_framebuffer_ = 0;

}


// this function is called frequently by CADisplayLink
-(void) iterateIOSView
{
    //[EAGLContext setCurrentContext:theEAGLContext];

    // pre iteration
    ios_tickBegin();
    ios_keysBegin();

    // iterate Haskell
    haskell_iterate();
    
    // post iteration
    ios_keysEnd();
    ios_tickEnd();
    
    
    // present multisample_framebuffer_:
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER_APPLE, resolve_framebuffer_);
    glBindFramebuffer(GL_READ_FRAMEBUFFER_APPLE, multisample_framebuffer_);
    glResolveMultisampleFramebufferAPPLE();
    
    const GLenum discards[]  = {GL_COLOR_ATTACHMENT0, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT};
    glDiscardFramebufferEXT(GL_READ_FRAMEBUFFER_APPLE, 3, discards);
    
    glBindRenderbuffer(GL_RENDERBUFFER, resolve_renderbuffer_);
    [theEAGLContext presentRenderbuffer:GL_RENDERBUFFER];

}

+ (Class)layerClass
{
    return [CAEAGLLayer class];
}


// handle touches

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    ios_touchesBegan( touches, event );
}

-(void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    ios_touchesMoved( touches, event );
}

-(void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    ios_touchesEnded( touches, event );
}

-(void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    ios_touchesCancelled( touches, event );
}

@end
