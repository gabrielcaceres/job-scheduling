!!! Module to permute binary string representing set
module m_setpermute
  implicit none

contains

  ! base from:
  ! http://alexbowe.com/popcount-permutations/
  ! http://graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation
  ! Permute set of binary flags
  function permset (v, s)
    integer, intent(in) :: v, s
    integer :: permset
    integer :: t, m
    !
    t = ior(v, v-1) + 1
    permset = ior(t,(ishft(iand(t, -t) / iand(v, -v), -1) - 1))
    ! select only bits in set size
    m = firstset(s)
    permset = iand(permset, m)
  end function permset
  !
  ! Create first permutation (or mask to use)
  function firstset (s)
    integer :: firstset
    integer, intent(in) :: s
    !
    firstset = ibset(0, s)
    firstset = firstset - 1    
  end function firstset


end module m_setpermute
