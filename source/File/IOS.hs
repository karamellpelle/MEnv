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
module File.IOS
  (
    fileStaticData,
    fileDynamicData,
    fileUser,
    fileTmp,

  ) where


import MyPrelude
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array


-- | fixme: do not use fixed arrays, instead peek foreign values
--          as in MEnv.Players.IOS !
valueMaxFilePathLength :: UInt
valueMaxFilePathLength = 
    255

foreign import ccall unsafe "ios_fileStaticData" ios_fileStaticData
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to read-only application data
fileStaticData :: FilePath -> IO FilePath
fileStaticData path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileStaticData ptrNameExt ptrDst 
                               (fI $ valueMaxFilePathLength + 1) >>= \value -> 
              case value of 
                  0   -> error $ "fileStaticData: no file '" ++ path ++ "'"
                  _   -> peekCString ptrDst




foreign import ccall unsafe "ios_fileDynamicData" ios_fileDynamicData
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to read-write application data
fileDynamicData :: FilePath -> IO FilePath
fileDynamicData path =
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileDynamicData ptrNameExt ptrDst 
                                (fI $ valueMaxFilePathLength + 1) >>= \value -> 
            case value of
                0   -> error $ "fileDynamicData: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst



foreign import ccall unsafe "ios_fileUser" ios_fileUser
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to user file directory
fileUser :: FilePath -> IO FilePath
fileUser path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileUser ptrNameExt ptrDst 
                         (fI valueMaxFilePathLength) >>= \value -> 
            case value of
                0   -> error $ "fileUser: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst




foreign import ccall unsafe "ios_fileTmp" ios_fileTmp
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to tmp file directory
fileTmp :: FilePath -> IO FilePath
fileTmp path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileTmp ptrNameExt ptrDst 
                    (fI valueMaxFilePathLength) >>= \value -> case value of
                0   -> error $ "fileTmp: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst





