think-cell public library
=========================

This library consists of several core C++ utilities that we at *think-cell Software* have developed and consider to be useful.
It mostly covers ranges, but you will see that it also contains other handy features.

We continuously improve our library as part of our daily work, and whenever we gain new insights, we add them as we go.
To get in touch with other programmers, we regularly [give talks about our ideas](https://www.think-cell.com/career/talks/overview.shtml), using this library as a reference.

We consider the library to be production-quality code, but it is important to know that we do not strive for stable interfaces. Being free of such constraints is an important requirement for further enhancements.

Clean and expressive code makes reasoning about it - and thus further progress - easier. Therefore, we adopt the latest language features quickly if it helps the case.

This library has been made publicly available as an example of modern C++ coding techniques and as a source of inspiration for other library writers. And of course because we are proud of it!

-------------
Contributions
-------------
If you propose a change that improves correctness or standard-conformance, we encourage you to make a pull request.
But please understand that, for the above-mentioned reasons, we are not keen on workarounds to accommodate outdated compilers. 
 
Does hacking our library give you a kick, and do you think you can contribute more? We are a friendly and driven bunch of C++ enthusiasts with a knack for elegant algorithms, and we are always looking for [new colleagues](https://www.think-cell.com/career).

------------------
Usage instructions
------------------
You need to have boost installed (we tested with [1.75.0](https://www.boost.org/users/history/version_1_75_0.html)) and the following compiler settings:

##### Visual C++ 19.28 (Visual Studio 2019 16.9)
* `/std:c++latest`

##### clang Apple LLVM 12.0.0 (Xcode 12.0)
* `-std=c++2a`

`range.example.cpp` provides a good entry point to get started quickly. If you want to see more examples, there are some unit tests in `tc/*.t.cpp`.
