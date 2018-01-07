// ------------------------------------------
// Copyright (c) 2017-2018 lpha-z
// https://github.com/lpha-z/lfl
// Distributed under the MIT software license
// ------------------------------------------
#ifndef LFL_LFL_HPP
#define LFL_LFL_HPP
#include <cstddef>
#include <iterator>
#include <utility>
#include <array>

namespace lfl
{
	// index_t
	using index_t = std::size_t;

	// index_tuple
	template<lfl::index_t... Index>
	struct index_tuple { };

	namespace detail
	{
		// construct_index_tuple
		template<class IndexTuple, lfl::index_t Step, bool Even>
		struct construct_index_tuple;

		template<lfl::index_t... Index, lfl::index_t Step>
		struct construct_index_tuple<lfl::index_tuple<Index...>, Step, true>
		{
			using type = lfl::index_tuple<Index..., ( Index + Step )...>;
		};

		template<lfl::index_t... Index, lfl::index_t Step>
		struct construct_index_tuple<lfl::index_tuple<Index...>, Step, false>
		{
			using type = lfl::index_tuple<Index..., ( Index + Step )..., Step * 2>;
		};

		// index_tuple_impl
		template<std::size_t N>
		struct index_tuple_impl : public lfl::detail::construct_index_tuple<typename lfl::detail::index_tuple_impl<N / 2>::type, N/2, N % 2 == 0> { };

		template<>
		struct index_tuple_impl<0>
		{
			using type = lfl::index_tuple<>;
		};

	} // detail

	// make_index_tuple
	template<std::size_t N>
	using make_index_tuple = typename lfl::detail::index_tuple_impl<N>::type;

	// array
	template<class T, std::size_t N>
	struct array
	{
		T elems[N];
		using value_type = T;
		using size_type = std::size_t;
		using difference_type = std::ptrdiff_t;
		using reference = value_type&;
		using const_reference = const value_type&;
		using pointer = T*;
		using const_pointer = const T*;
		using iterator = T*;
		using const_iterator = const T*;
		using reverse_iterator = std::reverse_iterator<iterator>;
		using const_reverse_iterator = std::reverse_iterator<const_iterator>;

		reference at( size_type n ) { return n < size() ? elems[n] : ( throw std::out_of_range( "lfl::index_array: index out of range" ), elems[n] ); }
		constexpr const_reference at( size_type n ) const { return n < size() ? elems[n] : ( throw std::out_of_range( "lfl::index_array: index out of range" ), elems[n] ); }

		reference operator[]( size_type n ) { return elems[n]; }
		constexpr const_reference operator[]( size_type n ) const { return elems[n]; }

		reference front() { return elems[0]; }
		constexpr const_reference front() const { return elems[0]; }

		reference back() { return elems[N - 1]; }
		constexpr const_reference back() const { return elems[N - 1]; }

		T* data() noexcept { return elems; }
		constexpr const T* data() const noexcept { return elems; }

		iterator begin() noexcept { return &elems[0]; }
		constexpr const_iterator begin() const noexcept { return &elems[0]; }

		iterator end() noexcept { return &elems[N]; }
		constexpr const_iterator end() const noexcept { return &elems[N]; }

		constexpr const_iterator cbegin() const noexcept { return &elems[0]; }
		constexpr const_iterator cend() const noexcept { return &elems[N]; }

		reverse_iterator rbegin() noexcept { return static_cast<reverse_iterator>( &elems[0] ); }
		constexpr const_reverse_iterator rbegin() const noexcept { return static_cast<const_reverse_iterator>( &elems[0] ); }

		reverse_iterator rend() noexcept { return static_cast<reverse_iterator>( &elems[N] ); }
		constexpr const_reverse_iterator rend() const noexcept { return static_cast<const_reverse_iterator>( &elems[N] ); }

		constexpr const_iterator crbegin() const noexcept { return static_cast<const_reverse_iterator>( &elems[0] ); }
		constexpr const_iterator crend() const noexcept { return static_cast<const_reverse_iterator>( &elems[N] ); }

		constexpr bool empty() const noexcept { return N == 0; }
		constexpr size_type size() const noexcept { return N; }
		constexpr size_type max_size() const noexcept { return N; }

		template<lfl::index_t... Index>
		void fill_impl( const_reference value, lfl::index_tuple<Index...> ) { *this = array { { ( static_cast<void>( Index ), value )... } }; }
		void fill( const_reference value ) { fill_impl( value, lfl::make_index_tuple<N>{} ); }

		template<lfl::index_t... Index>
		std::array<T, N> to_array( lfl::index_tuple<Index...> ) { return std::array<T, N> { { elems[Index]... } }; }
		constexpr operator std::array<T, N>() const { return to_array( lfl::make_index_tuple<N>{} ); }

		using const_reference_rawarray = const T(&)[N];
		constexpr operator const_reference_rawarray() const { return elems; }
	};

	namespace detail
	{
		// concat_impl
		template<class T, std::size_t N, std::size_t M, lfl::index_t... Index1, lfl::index_t... Index2>
		constexpr lfl::array<T, N + M> concat_impl( const T( &arr1 )[N], const T( &arr2 )[M], lfl::index_tuple<Index1...>, lfl::index_tuple<Index2...> ) {
			return lfl::array<T, N + M> { { arr1[Index1]..., arr2[Index2]... } };
		}

		// generate_impl
		template<std::size_t N, class Func, lfl::index_t... Index,
			class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<lfl::index_t>() ) ), N>>
			constexpr RetType generate_impl( const Func& func, lfl::index_tuple<Index...> ) {
			return RetType { { func( Index )... } };
		}

		// map_impl
		template<class T, std::size_t N, class Func, lfl::index_t... Index,
			class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>() ) ), N>>
			constexpr RetType map_impl( const T( &arr )[N], const Func& func, lfl::index_tuple<Index...> ) {
			return RetType { { func( arr[Index] )... } };
		}

		// zip_with_impl
		template<class T, class U, std::size_t N, class Func, lfl::index_t... Index,
			class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>(), std::declval<U>() ) ), N>>
			constexpr RetType zip_with_impl( const T( &arr1 )[N], const U( &arr2 )[N], const Func& func, lfl::index_tuple<Index...> ) {
			return RetType { { func( arr1[Index], arr2[Index] )... } };
		}

		// reduce_bp_loop
		template<class U, class T, std::size_t N, class BinaryOp>
		constexpr U reduce_bp_loop( const U& initial, const T( &arr )[N], const BinaryOp& op, std::size_t begin, std::size_t end ) {
			return begin + 1 == end ? op( initial, arr[begin] )
			                        : lfl::detail::reduce_bp_loop( 
			                              lfl::detail::reduce_bp_loop( initial, arr, op, begin, ( begin + end ) / 2 ),
			                              arr, op, ( begin + end ) / 2, end
			                          );
		}

		// pred_binaryop (for count_if)
		template<class T, class Predicate>
		struct pred_binaryop
		{
			const Predicate& pred;
			constexpr pred_binaryop( const Predicate& pred ) : pred( pred ) { }
			constexpr std::size_t operator()( std::size_t n, const T& t ) const { return pred( t ) ? n + 1 : n; }
		};


		// --- (for sort) ---

		// LMB
		constexpr std::size_t LMB_mask( std::size_t n, std::size_t mask ) { return n&mask ? n&mask : n; }
		template<std::size_t Bits = sizeof( std::size_t ) * 8 / 2>
		constexpr std::size_t LMB_loop( std::size_t n, std::size_t mul ) {
			return Bits == 0 ? n : LMB_loop<Bits / 2>( LMB_mask( n, ~( ( ( std::size_t( 1 ) << Bits ) - 1 ) * mul ) ), mul * ( ( std::size_t( 1 ) << Bits ) + 1 ) );
		}
		constexpr std::size_t LMB( std::size_t n ) { return LMB_loop( n, 1 ); }

		// bitonic_sort
		template<class T, std::size_t N, class Comparator>
		constexpr T sorter( const T( &arr )[N], std::size_t i, std::size_t j, const Comparator& comp ) {
			return j >= N ? arr[i] :
			       i < j  ? comp( arr[i], arr[j] ) ? arr[i] : arr[j]
			              : comp( arr[i], arr[j] ) ? arr[j] : arr[i];
		}
		template<class T, std::size_t N, class Comparator, lfl::index_t... Index>
		constexpr lfl::array<T, N> bitonic_sort_sort( const T( &arr )[N], std::size_t xor_val, const Comparator& comp, lfl::index_tuple<Index...> ) {
			return { lfl::detail::sorter( arr, Index, Index^xor_val, comp )... };
		}
		template<class T, std::size_t N, class Comparator>
		constexpr lfl::array<T, N> bitonic_sort_loop2( const T( &arr )[N], std::size_t sub_N, std::size_t subsub_N, const Comparator& comp ) {
			return subsub_N == 1 ? lfl::detail::bitonic_sort_sort( arr, sub_N * 2 - 1, comp, lfl::make_index_tuple<N>{} )
			                     : lfl::detail::bitonic_sort_sort( (const T(&)[N])lfl::detail::bitonic_sort_loop2( arr, sub_N, subsub_N / 2, comp ), sub_N / subsub_N, comp, lfl::make_index_tuple<N>{} );
		}
		template<class T, std::size_t N, class Comparator>
		constexpr lfl::array<T, N> bitonic_sort_loop1( const T( &arr )[N], std::size_t sub_N, const Comparator& comp ) {
			return sub_N == 1 ? lfl::detail::bitonic_sort_loop2( arr, 1, 1, comp )
			                  : lfl::detail::bitonic_sort_loop2( (const T(&)[N])lfl::detail::bitonic_sort_loop1( arr, sub_N / 2, comp ), sub_N, sub_N, comp );
		}

		// any_of_bp_loop
		template<class T, std::size_t N, class Predicate>
		constexpr bool any_of_bp_loop( const T( &arr )[N], const Predicate& pred, std::size_t begin, std::size_t end ) {
			return begin + 1 == end ? pred( arr[begin] )
			                        : lfl::detail::any_of_bp_loop( arr, pred, begin, ( begin + end ) / 2 )
			                            ? true
			                            : lfl::detail::any_of_bp_loop( arr, pred, ( begin + end ) / 2, end );
		}

		// all_of_bp_loop
		template<class T, std::size_t N, class Predicate>
		constexpr bool all_of_bp_loop( const T( &arr )[N], const Predicate& pred, std::size_t begin, std::size_t end ) {
			return begin + 1 == end ? pred( arr[begin] )
			                        : lfl::detail::all_of_bp_loop( arr, pred, begin, ( begin + end ) / 2 )
			                            ? lfl::detail::all_of_bp_loop( arr, pred, ( begin + end ) / 2, end )
			                            : false;
		}

		// any_of_bp_loop
		template<class T, std::size_t N, class Predicate>
		constexpr bool none_of_bp_loop( const T( &arr )[N], const Predicate& pred, std::size_t begin, std::size_t end ) {
			return begin + 1 == end ? !pred( arr[begin] )
			                        : lfl::detail::none_of_bp_loop( arr, pred, begin, ( begin + end ) / 2 )
			                            ? lfl::detail::none_of_bp_loop( arr, pred, ( begin + end ) / 2, end )
			                            : false;
		}

	}

	// concat
	template<class T, std::size_t N, std::size_t M>
	constexpr lfl::array<T, N + M> concat( const T( &arr1 )[N], const T( &arr2 )[M] ) {
		return lfl::detail::concat_impl( arr1, arr2, lfl::make_index_tuple<N>{}, lfl::make_index_tuple<M>{} );
	}

	// generate
	template<std::size_t N, class Func,
	         class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<lfl::index_t>() ) ), N>>
	constexpr RetType generate( const Func& func ) {
		return lfl::detail::generate_impl<N>( func, lfl::make_index_tuple<N>{} );
	}

	// map(transform)
	template<class T, std::size_t N, class Func,
	         class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>() ) ), N>>
	constexpr RetType map( const T( &arr )[N], const Func& func ) {
		return lfl::detail::map_impl( arr, func, lfl::make_index_tuple<N>{} );
	}

	// zip_with
	template<class T, class U, std::size_t N, class Func,
	         class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>(), std::declval<U>() ) ), N>>
	constexpr RetType zip_with( const T( &arr1 )[N], const U( &arr2 )[N], const Func& func ) {
		return lfl::detail::zip_with_impl( arr1, arr2, func, lfl::make_index_tuple<N>{} );
	}

	// reduce(accumulate)
	// a->[b]->(a->b->a)->a
	template<class U, class T, std::size_t N, class BinaryOp>
	constexpr U reduce( const U& initial, const T( &arr )[N], const BinaryOp& op ) {
		return lfl::detail::reduce_bp_loop( initial, arr, op, 0, N );
	}

	// count_if
	template<class T, std::size_t N, class Predicate>
	constexpr std::size_t count_if( const T( &arr )[N], const Predicate& pred ) {
		return lfl::reduce( std::size_t( 0 ), arr, lfl::detail::pred_binaryop<T, Predicate>( pred ) );
	}

	// --- (for filter) ---
	namespace detail{

		// pallarel_partial_reduce_loop
		// [a]->(a->a->a)->step=1->[a]
		template<class T, std::size_t N, class BinaryOp, lfl::index_t... Index>
		constexpr lfl::array<T, N> pallarel_partial_reduce_loop( const T( &arr )[N], const BinaryOp& op, std::size_t step, lfl::index_tuple<Index...> ) {
			return step * 2 >= N ? lfl::array<T, N> { { ( Index & step ? op( arr[Index], arr[( Index & ~( step - 1 ) ) - 1] ) : arr[Index] )... } }
			                     : lfl::detail::pallarel_partial_reduce_loop( 
			                           (const T(&)[N])lfl::array<T, N> { { ( Index & step ? op( arr[Index], arr[( Index & ~( step - 1 ) ) - 1] ) : arr[Index] )... } },
			                           op, step * 2, lfl::make_index_tuple<N>{}
			                       );
		}

		// lower_bound
		template<class T, std::size_t N>
		constexpr T lower_bound( const T( &arr )[N], const T& index, std::size_t begin, std::size_t end ) {
			return begin + 1 == end ? begin
			                        : index < arr[( begin + end ) / 2 - 1] ? lfl::detail::lower_bound( arr, index, begin, ( begin + end ) / 2 )
			                                                               : lfl::detail::lower_bound( arr, index, ( begin + end ) / 2, end );
		}

		// pred_size_t
		template<class T, class Predicate>
		struct pred_size_t
		{
			const Predicate& pred;
			constexpr pred_size_t( const Predicate& pred ) : pred( pred ) { }
			constexpr std::size_t operator()( T t ) const { return pred( t ) ? 1 : 0; }
		};

		// plus_size_t
		struct plus_size_t
		{
			constexpr std::size_t operator()( std::size_t a, std::size_t b ) const { return a + b; }
		};

		// find_nth
		template<class T, std::size_t N, class Predicate>
		struct find_nth
		{
			const T( &arr )[N];
			const lfl::array<std::size_t, N> count_arr;
			const Predicate& pred;
			constexpr find_nth( const T( &arr )[N], const Predicate& pred )
				: arr( arr )
				, count_arr(
				             lfl::detail::pallarel_partial_reduce_loop( 
				                (const std::size_t(&)[N])lfl::map( arr, lfl::detail::pred_size_t<T, Predicate>( pred ) ),
				                lfl::detail::plus_size_t{}, 1, lfl::make_index_tuple<N>{}
				             )
				           )
				, pred( pred ) { }
			constexpr T operator()( std::size_t n ) const {
				return n < count_arr[N - 1] ? arr[lfl::detail::lower_bound( (const std::size_t(&)[N])count_arr, n, 0, N )]
				                            : T();
			}
		};
	}

	// filter(copy_if)
	template<std::size_t M, class T, std::size_t N, class Predicate>
	constexpr lfl::array<T, M> filter( const T( &arr )[N], const Predicate& pred ) {
		return lfl::generate<M>( lfl::detail::find_nth<T, N, Predicate>( arr, pred ) );
	}

	// sort
	template<class T, std::size_t N, class Comparator>
	constexpr lfl::array<T, N> sort( const T( &arr )[N], const Comparator& comp ) {
		return lfl::detail::bitonic_sort_loop1( arr, lfl::detail::LMB( N - 1 ), comp );
	}
	// any_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool any_of( const T( &arr )[N], const Predicate& pred ) {
		return lfl::detail::any_of_bp_loop( arr, pred, 0, N );
	}

	// all_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool all_of( const T( &arr )[N], const Predicate& pred ) {
		return lfl::detail::all_of_bp_loop( arr, pred, 0, N );
	}

	// none_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool none_of( const T( &arr )[N], const Predicate& pred ) {
		return lfl::detail::none_of_bp_loop( arr, pred, 0, N );
	}

	// --- (for lfl::array) ---

	// concat
	template<class T, std::size_t N, std::size_t M>
	constexpr lfl::array<T, N + M> concat( const lfl::array<T, N>& arr1, const lfl::array<T, M>& arr2 ) {
		return lfl::detail::concat_impl( (const T(&)[N])arr1, (const T(&)[M])arr2, lfl::make_index_tuple<N>{}, lfl::make_index_tuple<M>{} );
	}

	// map(transform)
	template<class T, std::size_t N, class Func,
		class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>() ) ), N>>
		constexpr RetType map( const lfl::array<T, N>& arr, const Func& func ) {
		return lfl::detail::map_impl( (const T(&)[N])arr, func, lfl::make_index_tuple<N>{} );
	}

	// zip_with
	template<class T, class U, std::size_t N, class Func,
		class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>(), std::declval<U>() ) ), N>>
		constexpr RetType zip_with( const lfl::array<T, N>& arr1, const lfl::array<U, N>& arr2, const Func& func ) {
		return lfl::detail::zip_with_impl( (const T(&)[N])arr1, (const U(&)[N])arr2, func, lfl::make_index_tuple<N>{} );
	}

	// reduce(accumulate)
	// a->[b]->(a->b->a)->a
	template<class U, class T, std::size_t N, class BinaryOp>
	constexpr U reduce( const U& initial, const lfl::array<T, N>& arr, const BinaryOp& op ) {
		return lfl::detail::reduce_bp_loop( initial, (const T(&)[N])arr, op, 0, N );
	}

	// count_if
	template<class T, std::size_t N, class Predicate>
	constexpr std::size_t count_if( const lfl::array<T, N>& arr, const Predicate& pred ) {
		return lfl::reduce( std::size_t( 0 ), (const T(&)[N])arr, lfl::detail::pred_binaryop<T, Predicate>( pred ) );
	}

	// filter(copy_if)
	template<std::size_t M, class T, std::size_t N, class Predicate>
	constexpr lfl::array<T, M> filter( const lfl::array<T, N>& arr, const Predicate& pred ) {
		return lfl::generate<M>( lfl::detail::find_nth<T, N, Predicate>( (const T(&)[N])arr, pred ) );
	}

	// sort
	template<class T, std::size_t N, class Comparator>
	constexpr lfl::array<T, N> sort( const lfl::array<T, N>& arr, const Comparator& comp ) {
		return lfl::detail::bitonic_sort_loop1( (const T(&)[N])arr, lfl::detail::LMB( N - 1 ), comp );
	}
	// any_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool any_of( const lfl::array<T, N>& arr, const Predicate& pred ) {
		return lfl::detail::any_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}

	// all_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool all_of( const lfl::array<T, N>& arr, const Predicate& pred ) {
		return lfl::detail::all_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}

	// none_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool none_of( const lfl::array<T, N>& arr, const Predicate& pred ) {
		return lfl::detail::none_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}

	// --- ( another argument order ) ---

	// map(transform)
	template<class Func, class T, std::size_t N,
		class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>() ) ), N>>
		constexpr RetType map( const Func& func, const lfl::array<T, N>& arr ) {
		return lfl::detail::map_impl( (const T(&)[N])arr, func, lfl::make_index_tuple<N>{} );
	}

	// zip_with
	template<class Func, class T, class U, std::size_t N,
		class RetType = lfl::array<decltype( std::declval<Func>()( std::declval<T>(), std::declval<U>() ) ), N>>
		constexpr RetType zip_with( const Func& func, const lfl::array<T, N>& arr1, const lfl::array<U, N>& arr2 ) {
		return lfl::detail::zip_with_impl( (const T(&)[N])arr1, (const U(&)[N])arr2, func, lfl::make_index_tuple<N>{} );
	}

	// reduce(accumulate)
	// (a->b->a)->a->[b]->a
	template<class BinaryOp, class U, class T, std::size_t N>
	constexpr U reduce( const BinaryOp& op, const U& initial, const lfl::array<T, N>& arr ) {
		return lfl::detail::reduce_bp_loop( initial, (const T(&)[N])arr, op, 0, N );
	}

	// count_if
	template<class Predicate, class T, std::size_t N>
	constexpr std::size_t count_if( const Predicate& pred, const lfl::array<T, N>& arr ) {
		return lfl::reduce( std::size_t( 0 ), (const T(&)[N])arr, lfl::detail::pred_binaryop<T, Predicate>( pred ) );
	}

	// filter(copy_if)
	template<std::size_t M, class Predicate, class T, std::size_t N>
	constexpr lfl::array<T, M> filter( const Predicate& pred, const lfl::array<T, N>& arr ) {
		return lfl::generate<M>( lfl::detail::find_nth<T, N, Predicate>( (const T(&)[N])arr, pred ) );
	}

	// sort
	template<class Comparator, class T, std::size_t N>
	constexpr lfl::array<T, N> sort( const Comparator& comp, const lfl::array<T, N>& arr ) {
		return lfl::detail::bitonic_sort_loop1( (const T(&)[N])arr, lfl::detail::LMB( N - 1 ), comp );
	}
	// any_of
	template<class Predicate, class T, std::size_t N>
	constexpr bool any_of( const Predicate& pred, const lfl::array<T, N>& arr ) {
		return lfl::detail::any_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}

	// all_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool all_of( const Predicate& pred, const lfl::array<T, N>& arr ) {
		return lfl::detail::all_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}

	// none_of
	template<class T, std::size_t N, class Predicate>
	constexpr bool none_of( const Predicate& pred, const lfl::array<T, N>& arr ) {
		return lfl::detail::none_of_bp_loop( (const T(&)[N])arr, pred, 0, N );
	}
} // lfl

#endif // LFL_LFL_HPP
