" Set leader to space
let mapleader =" "

nnoremap <F5> :MundoToggle<CR>:call s:MundoPython('MundoRenderGraph()')<CR>

" Maximize current pane with <C-w>
nnoremap <silent><C-w>z :tabedit %<CR>
vnoremap <silent><C-w>z :tabedit %<CR>gv
inoremap <silent><C-w>z <C-o>:tabedit %<CR>
tnoremap <silent><C-w>z <Cmd>:tabedit %<CR>

" Ctrl-s
nmap <C-s> :up<CR>
vmap <C-s> <ESC>:up<CR>gv
imap <C-s> <C-o>:up<CR>

" Ctrl-k Ctrl-u
inoremap <C-k> <C-o>dg_
inoremap <C-u> <C-o>d^

" Ctrl-Up Ctrl-Down move paragraph
inoremap <C-Up> <C-o>{
nnoremap <C-Up> {
vnoremap <C-Up> {
inoremap <C-Down> <C-o>}
nnoremap <C-Down> }
vnoremap <C-Down> }


" delete words in insert mode
inoremap <C-BS> <C-W>
inoremap <C-Del> <C-o>dw

" backspace delete in visual mode
vnoremap <BS> <Del>

" terminal mappings
tnoremap <C-g> <C-\><C-n>
tnoremap <ESC>: <C-\><C-n>:


" indentation in Visual mode
vnoremap <S-Tab> <gv
vnoremap <Tab> >gv
