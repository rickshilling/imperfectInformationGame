#!/bin/bash
apt-get update
sudo apt-get install emacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
git clone https://github.com/rickshilling/imperfectInformationGame.git
curl -sSL https://get.haskellstack.org/ | sh
