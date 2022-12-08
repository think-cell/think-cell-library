
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "track_instance.h"
#include "../unittest.h"

UNITTESTDEF(track_instance) {
	struct SUnique : tc::track_unique_instance<SUnique> {};
	struct SOutermost : tc::track_outermost_instance<SOutermost> {};

	_ASSERT(!SUnique::instance());
	_ASSERT(!SOutermost::instance());
	{
		SUnique u;
		_ASSERT(SUnique::instance()==&u);
		_ASSERT(!SOutermost::instance());

		SOutermost o1;
		{
			SOutermost o2;
			_ASSERT(o1.is_outermost_instance());
			_ASSERT(!o2.is_outermost_instance());
		}
		_ASSERT(o1.is_outermost_instance());
	}

	_ASSERT(!SUnique::instance());
	_ASSERT(!SOutermost::instance());
}
