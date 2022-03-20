" Plugins
call plug#begin('~/.vim/plugged')
    " Theme stuff
    Plug 'vim-airline/vim-airline-themes'
    Plug 'vim-airline/vim-airline'
    Plug 'altercation/vim-colors-solarized'
    Plug 'lrustand/base16-vim'

    " Tmux stuff
    Plug 'roxma/vim-tmux-clipboard'
    Plug 'sunaku/tmux-navigate'

    " Completion stuff
    Plug 'folke/which-key.nvim', { 'branch': 'main' }
    Plug 'neovim/nvim-lspconfig'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'

    " Image view
    Plug 'lrustand/image.vim'

    " Undo tree
    Plug 'simnalamburt/vim-mundo'

    " Syntax
    Plug 'sirtaj/vim-openscad'
    Plug 'sheerun/vim-polyglot'
    Plug 'PotatoesMaster/i3-vim-syntax'

    " FZF
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'

    " Ag
    Plug 'rking/ag.vim'

    " Latex
    Plug 'xuhdev/vim-latex-live-preview'

    " Git (TODO do we need both?)
    Plug 'airblade/vim-gitgutter'
    Plug 'tpope/vim-fugitive'

    " Python REPL
    Plug 'jpalardy/vim-slime'

    " Nerdtree
    Plug 'scrooloose/nerdtree'
    "Plug 'jistr/vim-nerdtree-tabs'
    Plug 'lrustand/nerdtree-visual-selection'
    Plug 'Xuyuanp/nerdtree-git-plugin'
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

    " Other, TODO needs sorting and cleaning
    Plug 'neovim/python-client'
    Plug 'majutsushi/tagbar'
    Plug 'xolox/vim-misc'
    Plug 'kshenoy/vim-signature'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'scrooloose/nerdcommenter'
    Plug 'terryma/vim-multiple-cursors'
    Plug 'ntpeters/vim-better-whitespace'

    " Dev icons, always load last
    Plug 'ryanoasis/vim-devicons' " Requires NERD font: https://aur.archlinux.org/packages/nerd-fonts-dejavu-complete

call plug#end()

