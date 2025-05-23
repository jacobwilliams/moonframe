program interp_test
    use moon_frame_module, wp => moon_frame_wp

    implicit none

    type(moon_frame_interpolater) :: moon_pa
    real(wp),dimension(3,3) :: rot !! rotation matrix from J2000 to moon_pa
    real(wp) :: et !! ephemeris time (sec)
    integer :: i !! counter

    call moon_pa%initialize('data/moon_pa_2000_2100.csv', et0 = -1.0_wp, etf = 400000.0_wp)

    et = 0.0_wp
    rot = moon_pa%j2000_to_frame(et)

    write(*,*) ''
    write(*,*) 'rot at et=0.0:'
    do i = 1, 3
        write(*,*) rot(i,:)
    end do
    write(*,*) ''

    call moon_pa%destroy()

end program interp_test