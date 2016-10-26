compilation
-----------

ubuntu-linux:
sudo apt-get install haskell-platform
ghc -make ABFA.hs

arch-linux:
sudo yaourt -S haskell-text haskell-opengl
ghc --make ABFA.hs -package OpenGLRaw-1.5.0.1 -package GLURaw-1.4.0.2
