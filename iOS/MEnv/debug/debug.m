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
#import "debug.h"
#import <stdint.h>

#import "IOS.h"

void debug_dump_bundle()
{
    NSString* path = [[NSBundle mainBundle] bundlePath];
    
    // Enumerators are recursive
    NSDirectoryEnumerator *enumerator = [[[NSFileManager defaultManager] enumeratorAtPath:path] retain];

    NSString *filePath;

    while ((filePath = [enumerator nextObject]) != nil){
        printf("%s\n", [filePath UTF8String]);
        
    }

    [enumerator release];

}


void debug_dump_image(const char* name, uint8_t* data, uint wth, uint hth)
{
    printf("\ndump of %s. wth is %u, hth is %u:\n", name, wth, hth);
    uint i = 0;
    while (i != hth)
    {
        int j = 0;
        while (j != wth)
        {
            size_t ix = (i * wth + j) * 4;
            printf("(%#4x %#4x %#4x %#4x) ", data[ix], data[ix + 1],
                    data[ix + 2], data[ix + 3]);
            ++j;
        }
        printf("\n");
        ++i;
    }

}

/*
void null_f() {}

int main(int argc, char** argv)
{
    debug_filltexback("main(argc, argv)");
    ios_main( null_f, null_f);
    return 0;
}
*/

void debug_fill_bytes(uint8_t* buf, size_t size)
{
    uint8_t value = 0;
    for (size_t i = 0; i != size; ++i)
    {
        buf[i] = value++;
    }
}
