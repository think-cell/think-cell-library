
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "container_traits.h"
#include "subrange.h"
#include "storage_for.h"
#include "restrict_size_decrement.h"

namespace tc {

	template <typename T>
	struct range_filter_by_move_element : std::integral_constant<bool,
		tc::is_instance<std::basic_string,T>::value || tc::is_instance<std::vector,T>::value
	> {};

	template<typename Cont, typename Enable=void>
	struct range_filter_base;

	template<typename Cont>
	struct range_filter_base<Cont, std::enable_if_t<
		has_efficient_erase<Cont>::value
		|| has_mem_fn_lower_bound<Cont>::value
		|| has_mem_fn_hash_function<Cont>::value
	> >: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to subrange)

	private:
		Cont& m_cont;
		iterator m_itOutputEnd;

	public:
		explicit constexpr range_filter_base(Cont& cont) noexcept
			: m_cont(cont)
			, m_itOutputEnd(tc::begin(cont))
		{}

		constexpr range_filter_base(Cont& cont, iterator const& itStart) noexcept
			: m_cont(cont)
			, m_itOutputEnd(itStart)
		{}

	protected:
		constexpr void dtor() {
			tc::take_inplace( m_cont, m_itOutputEnd );
		}

	public:
		constexpr void keep(iterator it) & noexcept {
#ifdef _CHECKS
			auto const nDistance = tc::distance(m_itOutputEnd, it);
			_ASSERTE( 0<=nDistance );
			auto const rsize = constexpr_restrict_size_decrement(m_cont, nDistance, nDistance);
#endif
			m_itOutputEnd=m_cont.erase(m_itOutputEnd,it);
			++m_itOutputEnd;
#ifdef _CHECKS
			rsize.dtor();
#endif
		}

		///////////////////////////////////////////
		// range interface for output range
		// no deep constness (analog to subrange)

		constexpr iterator begin() const& noexcept {
			return tc::begin(tc::as_mutable(m_cont));
		}

		constexpr iterator end() const& noexcept {
			return m_itOutputEnd;
		}

		template< ENABLE_SFINAE, std::enable_if_t<!has_mem_fn_hash_function<SFINAE_TYPE(Cont)>::value>* = nullptr>
		constexpr void pop_back() & noexcept {
			_ASSERTE( m_itOutputEnd!=tc::begin(m_cont) );
			auto const rsize = constexpr_restrict_size_decrement(m_cont);
			--m_itOutputEnd;
			m_itOutputEnd=m_cont.erase(m_itOutputEnd);
			rsize.dtor();
		}
	};

	template<typename Cont>
	struct range_filter_base< Cont, std::enable_if_t<
		has_mem_fn_splice_after< Cont >::value
	> >: Cont, private tc::noncopyable {
		static_assert(tc::dependent_false<Cont>::value, "Careful: currently unused and without unit test");

		static_assert( tc::is_decayed< Cont >::value );
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to subrange)

	private:
		Cont& m_contInput;
		iterator m_itLastOutput;

	public:
		explicit constexpr range_filter_base(Cont& cont) noexcept
			: m_contInput(cont)
			, m_itLastOutput(cont.before_begin())
		{}

		explicit constexpr range_filter_base(Cont& cont, iterator const& itStart) noexcept
			: range_filter_base(cont)
		{
			for(;;) {
				auto it=tc::begin(m_contInput);
				if( it==itStart ) break;
				this->splice_after(m_itLastOutput,m_contInput.before_begin());
				m_itLastOutput=it;
			}
		}

	protected:
		constexpr void dtor() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

	public:
		constexpr void keep(iterator it) & noexcept {
			while( it!=tc::begin(m_contInput) ) m_contInput.pop_front();
			this->splice_after(m_itLastOutput,m_contInput.before_begin());
			m_itLastOutput=it;
		}
	};

	template<typename Cont>
	struct range_filter_base< Cont, std::enable_if_t<
		has_mem_fn_splice<Cont >::value
	> >: Cont, private tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		Cont& m_contInput;
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to subrange)

		explicit constexpr range_filter_base(Cont& cont) noexcept
			: m_contInput(cont)
		{}

		constexpr range_filter_base(Cont& cont, iterator const& itStart) noexcept
			: m_contInput(cont)
		{
			this->splice( tc::end(*this), m_contInput, tc::begin(m_contInput), itStart );
		}

	protected:
		constexpr void dtor() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

	public:
		constexpr void keep(iterator it) & noexcept {
			_ASSERTE( it!=tc::end(m_contInput) );
			this->splice( 
				tc::end(*this),
				m_contInput,
				m_contInput.erase( tc::begin(m_contInput), it )
			);
		}
	};

	template<typename Cont>
	struct range_filter_base<
		Cont,
		std::enable_if_t<range_filter_by_move_element<Cont>::value>
	>: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to subrange)

	protected:
		Cont& m_cont;
		iterator m_itOutput;

	private:
#ifdef _CHECKS
		iterator m_itFirstValid;
#endif

	public:
		explicit constexpr range_filter_base(Cont& cont) noexcept
			: m_cont(cont)
			, m_itOutput(tc::begin(cont))
#ifdef _CHECKS
			, m_itFirstValid(tc::begin(cont))
#endif
		{}

		explicit constexpr range_filter_base(Cont& cont, iterator itStart) noexcept
			: m_cont(cont)
			, m_itOutput(itStart)
#ifdef _CHECKS
			, m_itFirstValid(itStart)
#endif
		{}

	protected:
		constexpr void dtor() {
			tc::take_inplace( m_cont, m_itOutput );
		}

	public:
		constexpr void keep(iterator it) & noexcept {
#ifdef _CHECKS
			// Filter without reordering 
			_ASSERTE( 0<=tc::distance(m_itFirstValid, it) );
			m_itFirstValid=it;
			++m_itFirstValid;
#endif
			if (it != m_itOutput) { // self assignment with r-value-references is not allowed (17.6.4.9)
				*m_itOutput=tc_move_always(*it);
			}
			++m_itOutput;
		}

		///////////////////////////////////
		// range interface for output range
		// no deep constness (analog to subrange)

		constexpr iterator begin() const& noexcept {
			return tc::begin(tc::as_mutable(m_cont));
		}

		constexpr iterator end() const& noexcept {
			return m_itOutput;
		}

		constexpr void pop_back() & noexcept {
			_ASSERTE( tc::begin(m_cont)!=m_itOutput );
			--m_itOutput;
		}

		template <typename... Ts>
		constexpr void emplace_back(Ts&&... ts) & noexcept {
			_ASSERTE( tc::end(m_cont)!=m_itOutput );
			tc::renew(*m_itOutput, std::forward<Ts>(ts)...);
			++m_itOutput;
		}
	};

	template<typename Cont>
	struct range_filter : range_filter_base<Cont> {
		using range_filter_base<Cont>::range_filter_base;
		~range_filter() {
			this->dtor();
		}
	};

	// To use range_filter in constexpr context, use constexpr_range_filter, and call dtor() where the destructor would normally be called.
	template<typename Cont>
	struct constexpr_range_filter : range_filter_base<Cont> {
		using range_filter_base<Cont>::range_filter_base;
		using range_filter_base<Cont>::dtor;
	};

	template<typename Cont>
	struct range_filter_base<
		tc::subrange< Cont& >,
		std::enable_if_t<range_filter_by_move_element<Cont>::value>
	> {
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to subrange)

		explicit constexpr range_filter_base(tc::subrange< Cont& >& rng) noexcept : m_rng(rng) {
			_ASSERTE(tc::end(m_rng) == tc::end(Container())); // otherwise, we would need to keep [ end(m_rng), end(Container()) ) inside dtor
			m_orngfilter.ctor(Container(), tc::begin(rng));
		}

		constexpr void keep(iterator it) & noexcept {
			m_orngfilter->keep(it);
		}

		constexpr iterator begin() const& noexcept {
			return tc::begin(m_rng);
		}

		constexpr iterator end() const& noexcept {
			return tc::end(*m_orngfilter);
		}

		constexpr void pop_back() & noexcept {
			_ASSERTE(tc::end(*this) != tc::begin(*this));
			m_orngfilter->pop_back();
		}

	protected:
		constexpr void dtor() {
			auto& cont = Container();
			auto const nIndexBegin = tc::begin(m_rng) - tc::begin(cont);
			m_orngfilter->dtor(); // erases cont tail and invalidates iterators in m_rng
			m_orngfilter.dtor();
			m_rng = tc::drop_first(cont, nIndexBegin);
		}

	private:
		constexpr Cont& Container() const& noexcept {
			return tc::implicit_cast<Cont&>(m_rng.base_range());
		}

		tc::subrange< Cont& >& m_rng;
		tc::storage_for< tc::constexpr_range_filter<Cont> > m_orngfilter;
	};

	/////////////////////////////////////////////////////
	// filter_inplace

	template<typename Cont, typename Pred>
	void filter_inplace(Cont & cont, typename boost::range_iterator<Cont>::type it, Pred pred) noexcept {
		for (auto const itEnd = tc::end(cont); it != itEnd; ++it) {
			if (!tc::bool_cast(tc::invoke(pred, *it))) {
				tc::range_filter< tc::decay_t<Cont> > rngfilter(cont, it);
				++it;
				while (it != itEnd) {
					if (tc::invoke(pred, *it)) { // taking further action to destruct *it when returning false is legitimate use case, so do do not enforce const
						rngfilter.keep(it++); // may invalidate it, so move away first
					} else {
						++it;
					}
				}
				break;
			}
		}
	}

	template<typename Cont, typename Pred>
	void filter_inplace(Cont& cont, Pred&& pred) noexcept {
		tc::filter_inplace( cont, tc::begin(cont), std::forward<Pred>(pred) );
	}


}
