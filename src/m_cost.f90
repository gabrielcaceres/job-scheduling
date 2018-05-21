!!! Module to calculate cost as a function of time
module m_cost
  implicit none
  
contains

   ! Cost function (using reverse profit)
  elemental function cost (t, jobcost, deadline)
    integer :: cost
    integer, intent(in) :: t, jobcost, deadline
    !
    if ( t <= deadline ) then
       cost = jobcost
       ! cost = 0
    else
       ! cost = jobcost + (t - deadline)
       ! cost = jobcost
       cost = 0
    end if
    ! Minimum profit = 0; i.e. order is rejected
    ! cost = min(cost, 0)
    ! 
  end function cost
  
  ! Profit function (real valued)
  elemental function rprofit (t, jobprofit, deadline, tardycost)
    real :: rprofit
    real, intent(in) :: jobprofit,tardycost
    integer, intent(in) :: t, deadline
    !
    if ( t <= deadline ) then
       rprofit = jobprofit
    else
       rprofit = jobprofit - real(t - deadline)*tardycost
    end if
    ! Minimum profit = 0; i.e. order is rejected
    rprofit = max(rprofit, 0.)
    ! 
  end function rprofit

  ! Profit function
  elemental function iprofit (t, jobprofit, deadline)
    integer :: iprofit
    integer, intent(in) :: t, jobprofit, deadline
    !
    if ( t <= deadline ) then
       iprofit = jobprofit
    else
       iprofit = jobprofit - (t - deadline)
    end if
    ! Minimum profit = 0; i.e. order is rejected
    iprofit = max(iprofit, 0)
    ! 
  end function iprofit

  ! Set profit to zero if capacity exceeded
  elemental function capcheck (t, tmax)
    integer :: capcheck
    integer, intent(in) :: t, tmax
    !
    if ( t <= tmax ) then
       capcheck = 1
    else
       capcheck = 0
    end if
    ! 
  end function capcheck

  
  ! function linpenalty (t,d)
  !   integer :: linpenalty
  !   integer, intent(in) :: t,d
  !   ! 
  !   if ( t <= d ) then
  !      linpenalty = 0
  !   else
       
  !   end if  
  ! end function linpenalty

  
end module m_cost
