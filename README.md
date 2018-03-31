# evolang

### About this repository

This repository provides a Prolog implementation of an agent-based model for simulating how language systems originate and evolve over time. It is largely based on the work of Dr. Josefina Sierra-Santibañez (see References).

The model has been designed around a series of reusable modules so that it (or parts of it) can be employed by other researchers conducting experiments in evolutionary linguistics.

### Dependencies

This implementation uses [CIAO 1.10#8](http://ciao-lang.org/download_legacy.html), and the source code is compiled using the `ciaoc` command.

It is quite possible that the code in this repository does not directly work under CIAO 1.14 or other Prolog implementations. If so, the adaptation process should not be excessively hard for the user with some knowledge of Prolog.

### Running a simulation

For detailed documentation please refer to [this dissertation](http://hdl.handle.net/2117/77998). The simplest way to use this program is by running:

```$ ./run.sh runs mode iterations step agents```

The main script [run.sh](run.sh) requires 5 arguments:

* `runs` : number of complete simulations to be performed.

* `mode` : can be `emerge` (for emergence experiments) or `trans` (for transmission experiments).

* `iterations` : number of language games per simulation.

* `step` : interval to use when collecting statistics.

* `agents` : size of the population in each simulation.

After the each run is completed, each agent will write _[end of simulation]_. When the overall simulation is finished, two new directories will be created:
   
* `./run/X` : contains separate information of each run X. Grammars of each agent appear in `gram_1.txt`, `gram_2.txt`, ... , `gram_10.txt`; and `evol_com.txt` collects information on the communicative success and coherence of the population. Files `ad_N.txt` and `inv_N.txt` keep track of adoption and invention for agent N.
       
* `./plot` : contains plots which represent the overall process. The mean communicative success and coherence of the overall population is plotted, together with the average number of adoptions and inventions. Files are generated in `.png` and `.tex` formats.
  
Additionally, the included [Makefile](Makefile) also provides the following targets:

```$ make all``` : cleans and compiles targets.

```$ make clean``` : deletes all files produced by a simulation.

```$ make compile``` : compiles the source code using the CIAO compiler.

### References

1. J. Sierra-Santibañez. [_An Agent-Based Model Studying the Acquisition of a Language System of Logical Constructions_](https://www.aaai.org/ocs/index.php/AAAI/AAAI14/paper/view/8342/8441). In Proceedings of the Twenty-Eighth AAAI Conference on Artificial Intelligence, AAAI-2014, pages 350–357. AAAI Press, 2014.

2. J. Sierra-Santibañez. [_An Agent-Based Model of the Emergence and Transmission of a Language System for the Expression of Logical Combinations_](https://www.aaai.org/ocs/index.php/AAAI/AAAI15/paper/view/9348/9285). In Proceedings of the Twenty-Ninth AAAI Conference on Artificial Intelligence, AAAI-2015, pages 492–499. AAAI Press, 2015.

3. J. Ginés i Ametllé. [_Implementation of an agent-based model for studying the acquisition of language systems of logical constructions_](http://hdl.handle.net/2117/77998) (Bachelor's thesis). Polytechnic University of Catalonia (UPC), 2015.
