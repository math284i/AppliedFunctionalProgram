# Pretty trees
This projects is about creating pretty trees, that are well aligned. \
The project contains 2 parts. 
> Part 1 is generating tree's and their positions, which is defined in the paper entitled "Functional Pearls" \
> Part 2 is visualising the generated tree, using the library SharpVG.

### How to run
    cd PrettyTrees
    dotnet build
    dotnet run

There is currently two different trees inside *program.fs* when running the program (only one will be run at the moment) but this can be changed in *program.fs*. The outputted svg can be found relative to where the program is run. Which would typically be inside *bin/Debug/net8.0* as tree.svg

### How to test the program
Testing.fs has a function ``runTests`` which takes 0 arguments, and runs all the tests.
It can be ran from program.fs as shown below
````
    [<EntryPoint>]
    let main argv =
    runTests
    0
````
And then go back to __How to run__