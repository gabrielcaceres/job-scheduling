!!! Program to schedule n jobs using Help & Karp algorithm
!   uses binary flag for each job as hash key
program hk_schedule
  use m_setpermute
  use m_cost
  implicit none
  integer, parameter :: njobs = 5                    !number of jobs
  integer, parameter :: tmax = 8                    !number of jobs
  ! ****
  ! Allocatable to use heap instead of stack to avoid segfaults for large arrays
  integer, allocatable,dimension(:) :: costset, lastjob, tset
  integer, dimension(njobs) :: jobseq, jobcost, tau, due
  integer, dimension(njobs) :: profitgain
  ! integer :: setsize, set, set0, prevset
  integer :: setsize
  integer :: set, set0, prevset
  integer :: newcost, extracost, mincost, finalcost
  integer :: ijob, mincostjob, nextjob
  integer :: caplimit

  
  ! ! job cost
  ! data jobcost   / 4,2,1,3,2,4,2,1,3,2,4,2,1,3,2,4,2,1,3,2,4,2,1,3,2,4,2,1 /!,3,2 /
  ! ! processing time
  ! data tau       / 2,2,1,4,3,2,2,1,4,3,2,2,1,4,3,2,2,1,4,3,2,2,1,4,3,2,2,1 /!,4,3 /
  ! ! deadline for full profit
  ! data due       / 4,5,4,7,6,4,5,4,7,6,4,5,4,7,6,4,5,4,7,6,4,5,4,7,6,4,5,4 /!,7,6 /
  
  ! job cost
  data jobcost     / 5,20,10,30,20 /
  ! processing time
  data tau  / 1,2,3,3,5 /
  ! deadline for full profit
  data due     / 4,5,4,7,6 /

  
  ! flip sign for profit
  jobcost = -jobcost
  ! 
  
  allocate(costset(2**njobs-1), lastjob(2**njobs-1), tset(2**njobs-1))
  ! costset = huge(mincost)

  ! First iteration; single job only
  setsize = 1
  write(*,*) setsize
  set0 = firstset(setsize)
  set = set0
  ijob = 1
  do while (set >= set0)
     costset(set) = profit(tau(ijob), jobcost(ijob), due(ijob))
     lastjob(set) = ijob
     tset(set) = tau(ijob)
     ijob = ijob + 1
     set = permset(set, njobs)
  end do

  ! Remaining iterations relying on previous stages
  do setsize = 2, njobs
     write(*,*) setsize
     set0 = firstset(setsize)
     set = set0
     ! Iterate through possible set permutations
     do while (set >= set0)
        mincost = huge(mincost)
        ! Find jobs in current set and get total production time
        ! tset = 0
        do ijob = 1, njobs
           if ( btest(set,ijob-1) ) then
              prevset = ibclr(set,ijob-1)
              tset(set) = tset(prevset) + tau(ijob)
              exit
           end if
        end do
        ! Find minimum cost last job
        caplimit = capcheck(tset(set), tmax)
        do ijob = 1, njobs
           if ( btest(set,ijob-1) ) then
              !Previous set with current job omitted
              prevset = ibclr(set,ijob-1)
              !Cost of adding current job
              extracost = caplimit*cost(tset(set), jobcost(ijob), due(ijob))
              newcost = costset(prevset) + extracost
              ! Get minimum cost job to do last
              if ( newcost < mincost ) then
                 mincost = newcost
                 mincostjob = ijob
              end if
           end if
        end do
        ! Best job & cost to do last given set
        costset(set) = mincost
        lastjob(set) = mincostjob
        ! Permute set for next iteration
        set = permset(set, njobs)
     end do
  end do
  !
  set = firstset(njobs)
  finalcost = costset(set)      !Best calculated cost
  ! Backtrack to find best job sequence
  do ijob = njobs, 1, -1
     nextjob = lastjob(set)
     jobseq(ijob) = nextjob
     profitgain(ijob) = capcheck(tset(set), tmax)*cost(tset(set), jobcost(nextjob), due(nextjob))
     set = ibclr(set,nextjob-1)
  end do

  write(*,*)
  write(*,*) finalcost
  write(*,*) jobseq
  write(*,*) profitgain
  write(*,*)

  deallocate(costset, lastjob, tset)
  
end program hk_schedule
