program interp_test
    use moon_frame_module, wp => moon_frame_wp

    implicit none

    type(moon_frame_interpolater) :: moon_pa
    real(wp),dimension(3,3) :: rot !! rotation matrix from J2000 to moon_pa
    real(wp) :: et !! ephemeris time (sec)
    integer :: i !! counter

    call moon_pa%initialize('data/moon_pa_2000_2100.csv')

    et = 0.0_wp
    call moon_pa%j2000_to_frame(et, rot)

    write(*,*) ''
    write(*,*) 'rot at et=0.0:'
    do i = 1, 3
        write(*,*) rot(i,:)
    end do
    write(*,*) ''

    call moon_pa%destroy()

end program interp_test