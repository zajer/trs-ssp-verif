# trs-ssp-verif
## What is it?
This library is meant to verify results produced by `tracking_bigraph`<sup>1</sup> library and transformed by `ssp_bridge`<sup>2</sup>. It verifies whether a scenario described by a walk (in graph theory sense) can be implemented as a set of continuous (in the meaning that there will be no unplanned periods of "breaks") sequences of actions.

<sup>1</sup>https://github.com/zajer/trs<br/>
<sup>2</sup>https://github.com/zajer/trs-ssp-bridge/

## Basic usage
Use case for this software is presented in ``examples`` folder. Basically if you design a system and its behavior with libraries mentioned earlier you can pass it into ``verify_visjs`` program to check if the system is correct <sup>3</sup>. Finally you can pass the verification output to some visualizer, a basic one (compatible with ``verify_visjs`` program) is available at: https://github.com/zajer/trs-ssp-frontend.

<sup>3</sup>To be precise, it checks whether there are some typical errors rather than formally verifies correctness of a system.
