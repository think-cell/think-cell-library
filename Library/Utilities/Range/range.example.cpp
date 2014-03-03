#include "../Range.h"
#include <vector>
#include <boost/range/adaptors.hpp>

namespace {

//---- Basic ------------------------------------------------------------------------------------------------------------------
void basic () {
   using namespace RANGE_PROPOSAL_NAMESPACE;

   int av[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
   auto v = std::vector<int> (av, av+sizeof(av)/sizeof(int)); 
   //std::vector<int> v = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

   auto evenvr = tc::filter(v, [](const int& v){ return (v%2==0);});

   for (auto& v: evenvr) {
      std::cout << v << ", ";
   }
   std::cout << std::endl;
}

//---- Generator Range --------------------------------------------------------------------------------------------------------
namespace {
	struct generator_range {
		template< typename Func >
		void operator()( Func func ) {
			for(int i=0;i<50;++i) {
				func(i);
			}
		}
	};
}

void ex_generator_range () {
	using namespace RANGE_PROPOSAL_NAMESPACE;
	for_each( tc::filter( generator_range(), [](int i){ return i%2==0; } ), [](int i) {
		std::cout << i << ", ";
	});
	std::cout << std::endl;
}

//---- Generator Range (with break) -------------------------------------------------------------------------------------------
namespace {
	struct generator_range_break {
		template< typename Func >
		RANGE_PROPOSAL_NAMESPACE::break_or_continue operator()( Func func ) {
			using namespace RANGE_PROPOSAL_NAMESPACE;
			for(int i=0;i<5000;++i) {
				if (func(i)==break_) { return break_; }
			}
			return continue_;
		}
	};
}

void ex_generator_range_break () {
	using namespace RANGE_PROPOSAL_NAMESPACE;
	for_each( tc::filter( generator_range_break(), [](int i){ return i%2==0; } ), [](int i) -> break_or_continue {
		std::cout << i << ", ";
		return (i>=50)? break_ : continue_;
	});
	std::cout << std::endl;
}

//---- Stacked filters --------------------------------------------------------------------------------------------------------
void stacked_filters() {
	using namespace RANGE_PROPOSAL_NAMESPACE;
	for_each( tc::filter( tc::filter( tc::filter(
								generator_range_break(),
								[](int i){ return i%2!=0; } ),
								[](int i){ return i%3!=0; } ),
								[](int i){ return i%5!=0; } )
			, [](int i) -> break_or_continue
	{
		std::cout << i << ", ";
		return (i>25)? break_ : continue_;
	});
	std::cout << std::endl;
}

}

int main() {
	std::cout << "-- Running Examples ----------" << std::endl;

	basic();
	ex_generator_range();
	ex_generator_range_break();
	stacked_filters();

	using namespace RANGE_PROPOSAL_NAMESPACE;

	int av[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
	auto v = std::vector<int> (av, av+sizeof(av)/sizeof(int)); 
  
	//---- N3752 filters examples  ----------------------------------------------------------------------------------------------

	auto r =  tc::filter( tc::filter( tc::filter(
								v,
								[](int i){ return i%2!=0; } ),
								[](int i){ return i%3!=0; } ),
								[](int i){ return i%5!=0; } );

	for (auto it = std::begin(r),
				end = std::end(r);
		it != end;
		++it)
	{
		std::cout << *it << ", ";
	}
	std::cout << std::endl;

	//---- boost for comparison -------------------------------------------------------------------------------------------------

	auto br = v | boost::adaptors::filtered([](int i){ return i%2!=0; })
				| boost::adaptors::filtered([](int i){ return i%3!=0; })
				| boost::adaptors::filtered([](int i){ return i%5!=0; });


	for (auto it = std::begin(br),
		end = std::end(br);
		it != end;
		++it)
	{
		std::cout << *it << ", ";
	}
	std::cout << std::endl;
	
	std::cout << "-- Done ----------" << std::endl;

	return EXIT_SUCCESS;
}



