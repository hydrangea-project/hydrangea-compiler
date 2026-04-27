bash -lc 'source ~/use-llvm.sh 15; cd ~/Repos/hydrangea-compiler/; NBODY_N=10000 cabal --project-dir="$HOME/Repos/hydrangea-compiler/bench/repa" run repa-bench -- bench nbody'
