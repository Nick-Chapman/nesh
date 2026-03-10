# nesh

NES Emulator in Stateful Haskell.

This is my third attempt at a NES emulator in Haskell.

My first attempt in 2019 was [honesty](https://github.com/Nick-Chapman/honesty). It was able to play Donkey Kong, albeit rather slowly, but never got the scrolling working enough to play the classics such as SMB. In the end, the project ground to a halt, engulfed by its own complexity, and disappointment with the FPS achieved.

Following a more successful emulation project of the 1978 arcade [space invaders](https://github.com/Nick-Chapman/space-invaders) which explored speed-ups via static recompilation, I returned for my second attempt at NES in 2022 with [amnesty](https://github.com/Nick-Chapman/amnesty) thinking I might pull the same speed-up tricks in NES. Mistake. Static recompilation adds a whole dimension of complexity which is unlikely to even have much benefit for NES since probably around 3/4 of the computational effort is required just for the PPU. And really having a working emulator is a requisite starting point. I didn't have this. Abort. Didn't achieve much here.

Fast forward a few more years to 2026 and I find myself nerd-sniped by the awesome [emudevz](https://github.com/afska/emudevz) by Rodrigo Alfonso. This is a game about building an NES emulator. Really! It is available free on steam or in-browser. Lovingly crafted, you are led level by level through emulation of the CPU, PPU and APU. You code in JavaScript, and at each level you must pass a bank of unit test to proceed. Such fun. Although JavaScript. To be honest, I haven't completed the APU levels yet, but I did finish the PPU. And so now armed with my new found understanding of the PPU details, it seemed only right to have another crack at a Haskell implementation...

Nesh has surpassed my previous attempts. Scrolling is basically working, although I didn't yet port the _loopy_ register implementation from the JavaScript. There is still no sound. And still not quite at the required speed -- I wish I knew how to optimise Haskell. On the plus side, I think my Haskell is neat and compact.

I am not sure it I have reached a full stop again, or if I will continue. But I thought at least I should write this README as a record of my progress.
