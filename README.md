# upenn-cis194-spring23

## List

- [x] [week0]
- [x] [week1]
- [x] [week2]
- [x] [week3]
- [x] [week4]
- [x] [week5]
- [x] [week6]
- [ ] [week7]
- [ ] [week8]
- [ ] [week9]
- [ ] [week10]
- [ ] [week11]
- [ ] [week12]

## build haskell enviornment

```sh

# install haskell language server ..
sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## config hls server

安装支持 ghc 9.2.5 版本的 hls

```sh
ghcup install hls 1.9.0.0
ghcup set hls 1.9.0.0
```

```sh
# /home/work/upenn-cis194-spring23/week02/homework
stack ghci Exercises.hs
main
:r
```
