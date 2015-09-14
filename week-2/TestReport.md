#Test-Report Week 2

This file contains the report concerning the exercises/tests for Week 2 of the course Software Specification and Testing at UvA.
Group members are stated in the readme.md.

## Recognizing triangles
Assignment:

Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:
Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

Indicate how you tested or checked the correctness of the program.

-Correctness test of the program was done by testing one valid case for each possible Shape.

##Recognizing Permutations

Assignment:

Write a function "isPermutation" that returns True if its arguments are permutations of each other.
Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?

Can you automate the test process? Use the techniques presented in this week's lecture.

-The test process could have been automated by generating a list of random int's (as in the code provided by the lecturer), shuffling their order such that the list was not the same (utilizing system.random.shuffle). 
Afterwards running isPermutation as a post condition using "testPost" on the original list and the list that was shuffled.
We chose hspec with a small sample size instead.

##Recognizing and generating derangements

Assignment:

Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement of another one.
Give a Haskell implementation of a function deran that generates a list of all derangements of the list [0..n-1].
Next, define some testable properties for the isDerangement function, and use some well-chosen integer lists to test isDerangement.

Can you automate the test process?
-Automating the test process can be done. We have a property showing that it is possible. Information on why we did not can be found in Lab2.hs

##Implementing and testing IBAN validation

Assignment:

The International Bank Account Number (IBAN) was designed to facility international money transfer, to uniquely identify bank accounts worldwide. 
It is described in this document. The document also defines a procedure for validating IBAN codes. 
Write a function "iban" that implements this validation procedure.
Next, test your implementation using some suitable list of examples.

Can you automate the test process?
-We could try validation the function by running generated IBANs through it. 
However as the function is designed to do exactly that (check for validity of IBANs)
how would we know the function is doing it's job? (Who guards the guards)
In this case we could use hoare logic to proof the correctness of the function.