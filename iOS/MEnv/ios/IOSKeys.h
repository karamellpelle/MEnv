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

void ios_initKeys();


void ios_touchesBegan(NSSet* touches, UIEvent* event);

void ios_touchesMoved(NSSet* touches, UIEvent* event);

void ios_touchesEnded(NSSet* touches, UIEvent* event);

void ios_touchesCancelled(NSSet* touches, UIEvent* event);


void ios_keysBegin();

void ios_keysEnd();


uint ios_keysTouchHandlePointTouched(float* , float* );

uint ios_keysTouchHandlePointReleased(float* , float* );

uint ios_keysTouchHandlePointDrag(double* ticks, float* x, float* y, float* x1, float* y1);

uint ios_keysTouchHandlePointDrop(double* ticks, float* x, float* y, float* x1, float* y1);  




uint ios_keysTouchHandleCircleTouched(float* , float* , float* );

uint ios_keysTouchHandleCircleReleased(float* , float* , float* );

uint ios_keysTouchHandleCircleDrag(double* ticks, float* x, float* y, float* d, float* x1, float* y1, float* d1);

uint ios_keysTouchHandleCircleDrop(double* ticks, float* x, float* y, float* d, float* x1, float* y1, float* d1);



uint ios_keysTouchHandleButtonA(float* x, float* y);

uint ios_keysTouchHandleButtonB(float* x, float* y);



void ios_keysAccel(float* x, float *y, float* z);

void ios_keysGyro(float* x, float* y, float* z);

//void ios_keysAccelRelative(float* x, float *y, float* z); // using CMDeviceMotion

//void ios_keysGyroRelative(Mat3* );    // using CMDeviceMotion




