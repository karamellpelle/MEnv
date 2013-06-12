--
--   Copyright (c) 2013, Carl Joachim Svenn
--   All rights reserved.
--   
--   Redistribution and use in source and binary forms, with or without 
--   modification, are permitted provided that the following conditions are met:
--   
--       1. Redistributions of source code must retain the above copyright notice, this 
--          list of conditions and the following disclaimer.
--       2. Redistributions in binary form must reproduce the above copyright notice, 
--          this list of conditions and the following disclaimer in the documentation 
--          and/or other materials provided with the distribution.
--   
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
--   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
--   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
--   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
--   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
--   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
--   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
--   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
--   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
module OpenGL.IOS.Ext where

import OpenGL.IOS.Types
import Foreign.Ptr
import Foreign.C.Types

-- GL_IMG_texture_compression_pvrtc
gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = 0x8C00

gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = 0x8C01

gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02

gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03

-- GL_OES_packed_depth_stencil
gl_DEPTH_STENCIL_OES :: GLenum
gl_DEPTH_STENCIL_OES = 0x84F9

gl_UNSIGNED_INT_24_8_OES :: GLenum
gl_UNSIGNED_INT_24_8_OES = 0x84FA

gl_DEPTH24_STENCIL8_OES :: GLenum 
gl_DEPTH24_STENCIL8_OES = 0x88F0

-- | GLvoid glDiscardFramebufferEXT(GLenum target, GLsizei numAttachments, const GLenum *attachments)
foreign import ccall unsafe "glDiscardFramebufferEXT" glDiscardFramebufferEXT
    :: GLenum -> GLsizei -> Ptr GLenum -> IO ()

-- | GLvoid glProgramUniform1iEXT(GLuint program, GLint location, GLint x)
foreign import ccall unsafe "glProgramUniform1iEXT" glProgramUniform1iEXT
    :: GLuint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform2iEXT(GLuint program, GLint location, GLint x, GLint y)  
foreign import ccall unsafe "glProgramUniform2iEXT" glProgramUniform2iEXT
    :: GLuint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform3iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z)  
foreign import ccall unsafe "glProgramUniform3iEXT" glProgramUniform3iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform4iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z, GLint w)
foreign import ccall unsafe "glProgramUniform4iEXT" glProgramUniform4iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform1fEXT(GLuint program, GLint location, GLfloat x)  
foreign import ccall unsafe "glProgramUniform1fEXT" glProgramUniform1fEXT
    :: GLuint -> GLint -> GLfloat -> IO ()

-- | GLvoid glProgramUniform2fEXT(GLuint program, GLint location, GLfloat x, GLfloat y)
foreign import ccall unsafe "glProgramUniform2fEXT" glProgramUniform2fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform3fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z)  
foreign import ccall unsafe "glProgramUniform3fEXT" glProgramUniform3fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform4fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w)  
foreign import ccall unsafe "glProgramUniform4fEXT" glProgramUniform4fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

gl_WRITE_ONLY_OES :: GLenum
gl_WRITE_ONLY_OES = 0x88B9

gl_BUFFER_ACCESS_OES :: GLenum 
gl_BUFFER_ACCESS_OES = 0x88BB

gl_BUFFER_MAPPED_OES :: GLenum
gl_BUFFER_MAPPED_OES = 0x88BC

gl_BUFFER_MAP_POINTER_OES :: GLenum
gl_BUFFER_MAP_POINTER_OES = 0x88BD

-- | void glGetBufferPointervOES (GLenum target, GLenum pname, GLvoid **params);
foreign import ccall unsafe "glGetBufferPointervOES" glGetBufferPointervOES
    :: GLenum -> GLenum -> Ptr (Ptr a) -> IO ()

-- | GLvoid* glMapBufferOES (GLenum target, GLenum access);
foreign import ccall unsafe "glMapBufferOES" glMapBufferOES
    :: GLenum -> GLenum -> IO (Ptr GLvoid)

-- | GLboolean glUnmapBufferOES (GLenum target);
foreign import ccall unsafe "glUnmapBufferOES" glUnmapBufferOES
    :: GLenum -> IO GLboolean

-- | GLvoid glBindVertexArrayOES(GLuint array);  
foreign import ccall unsafe "glBindVertexArrayOES" glBindVertexArrayOES
    :: GLuint -> IO ()

-- | GLvoid glDeleteVertexArraysOES(GLsizei n, const GLuint *arrays);  
foreign import ccall unsafe "glDeleteVertexArraysOES" glDeleteVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLvoid glGenVertexArraysOES(GLsizei n, GLuint *arrays);  
foreign import ccall unsafe "glGenVertexArraysOES" glGenVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLboolean glIsVertexArrayOES(GLuint array);
foreign import ccall unsafe "glIsVertexArrayOES" glIsVertexArrayOES
    :: GLuint -> IO GLboolean


