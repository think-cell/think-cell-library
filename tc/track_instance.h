
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"

namespace tc {
	namespace no_adl {
		template <typename Derived>
		struct track_instance_base {
			static Derived* instance() noexcept {
				return static_cast<Derived*>(c_ptib); 
			}
			void set_instance() & noexcept {
				_ASSERT(!c_ptib);
				c_ptib=this;
			}
			void reset_instance() const & noexcept {
				_ASSERT(is_instance());
				c_ptib=nullptr;
			};
			bool is_instance() const& noexcept {
				return c_ptib==this;
			}
		private:
			static track_instance_base* c_ptib;
		};
		template <typename Derived>
		track_instance_base<Derived>* track_instance_base<Derived>::c_ptib = nullptr;

		template <typename Derived>
		struct track_unique_instance : private track_instance_base<Derived> {
			friend struct track_instance_base<Derived>; // for cast to Derived

			track_unique_instance() noexcept {
				this->set_instance();
			}
			~track_unique_instance() noexcept {
				this->reset_instance();
			}
			using track_instance_base<Derived>::instance;
		};

		template <typename Derived>
		struct track_outermost_instance : private track_instance_base<Derived> {
			friend struct track_instance_base<Derived>; // for cast to Derived

			track_outermost_instance() noexcept {
				if (!this->instance()) {
					this->set_instance();
				}
			}
			~track_outermost_instance() noexcept {
				reset_outermost_instance();
			}
			using track_instance_base<Derived>::instance;

			bool is_outermost_instance() const& noexcept {
				return this->is_instance();
			}

		protected:
			bool reset_outermost_instance() & noexcept { // allow user to reset early in Derived dtor to embrace reentrance
				if (is_outermost_instance()) {
					this->reset_instance();
					return true;
				} else {
					return false;
				}
			}
		};
	}
	using no_adl::track_unique_instance;
	using no_adl::track_outermost_instance;
}
