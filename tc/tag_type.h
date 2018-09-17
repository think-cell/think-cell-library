
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#define DEFINE_TAG_TYPE(class_name) \
	struct class_name final { explicit class_name() noexcept = default; };

#define DEFINE_TEMPLATE_TAG_TYPE(class_name) \
	template<typename T> \
	DEFINE_TAG_TYPE(class_name)