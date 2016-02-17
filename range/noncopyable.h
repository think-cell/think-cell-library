#pragma once

namespace tc {
	namespace noncopyable_adl_barrier
	{
		// derived classes can be moved, but not copied
		struct noncopyable {
		protected:
			noncopyable() {};

			noncopyable(noncopyable const&) = delete;
			noncopyable& operator=(noncopyable const&) = delete;
			noncopyable(noncopyable &&) noexcept = default;
			noncopyable& operator=(noncopyable &&) noexcept = default;
		};
	}
	using noncopyable_adl_barrier::noncopyable;

	namespace nonmovable_adl_barrier {
		struct nonmovable {
		protected:
			nonmovable() = default;
			~nonmovable() = default;

			nonmovable(nonmovable const&) = delete;
			nonmovable& operator=(nonmovable const&) = delete;
			nonmovable(nonmovable &&) = delete;
			nonmovable& operator=(nonmovable &&) = delete;
		};
	}
	using nonmovable_adl_barrier::nonmovable;
}
