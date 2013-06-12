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
#import <GameKit/GameKit.h>


void ios_initPlayers();

void ios_playersAuthenticateLocalPlayer();

//uint ios_playersHandleLocalPlayer(char* ptr_id, uint ptr_id_len, char* ptr_alias, uint ptr_alias_len);

void ios_playersSendAchievement(const char* cat, float alpha);

// types.h: typedef long long int64_t;
void ios_playersSendScore(const char* ach, long long score);





// this is a Objective-C class encapsulating archive functionality
@interface IOSPlayersArchive : NSObject
{
    NSString* path_;
    //NSMutableArray* array_achievement_;
    //NSMutableArray* array_score_;
    
}

-(id) init;

-(void) pushGKScore:(GKScore*) score;

-(void) pushGKAchievement:(GKAchievement*) achievement;

-(void) sendArchive;

-(void) clear;

@end

extern IOSPlayersArchive* theIOSPlayersArchive;

