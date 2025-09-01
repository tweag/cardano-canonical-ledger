
In this file we provide a list of implementation choices that were made.
So in case if anyone would suggest an improvement over the current status
they may check why current solution was chosen and support new idea with
a description why it would solve current concern better then the current one.


# Decoding and encoding

At this point we use `binary` library as a wrapper for the `bytestring` access.
We find it not the best choice, while binary library is quite nice it may not
work best for our scenario because it's optimized for streaming writing and reading
when we do not know the size of data in advance. However the file format is optimized
and structured in the way that we know either exact sizes or the boundaries. As a
result we may work with more efficient structures that removes some checks.
