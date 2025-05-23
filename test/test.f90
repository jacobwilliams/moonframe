
!***************************************************************************
!>
!  Tests for the moon frame interpolater.

    program test

    use moon_frame_module, wp => moon_frame_wp
    use iso_fortran_env
    use moon_frame_spice_interface

#ifdef HAS_SPICELIB

    implicit none

    type(moon_frame_interpolater) :: moon_pa
    real(wp),dimension(3,3) :: rot
    real(wp),dimension(3,1) :: r
    real(wp) :: et
    real(wp) :: max_r_error, r_error
    real(wp) :: tstart, tend
    real(real64),dimension(3,3) :: rot_true

    ! load the kernels:
    call furnsh ( './kernels/moon_de440_250416.tf' )
    call furnsh ( './kernels/moon_pa_de440_200625.bpc'   )
    call furnsh ( './kernels/pck00011.tpc'   )
    call furnsh ( './kernels/naif0012.tls'   )

    call moon_pa%initialize('data/moon_pa_2000_2100.csv')

    r(:,1) = 1737.4_wp * unit([1000.0_wp, 1000.0_wp, 1000.0_wp])  ! point on surface of moon

    et = 0.0_wp
    call moon_pa%j2000_to_frame(et, rot)
    call from_j2000_to_moon_pa(real(et, real64), rot_true)
    write(*,*) ''
    write(*,*) 'rot error at et=0', rot_true - rot
    write(*,*) 'pos error: ', norm2(matmul(rot_true, r) - matmul(rot, r))

    et = 21600.0_wp ! halfway between first two points
    call moon_pa%j2000_to_frame(et, rot)
    call from_j2000_to_moon_pa(real(et, real64), rot_true)
    write(*,*) ''
    write(*,*) 'rot error at et=21600', rot_true - rot
    write(*,*) 'pos error: ', norm2(matmul(rot_true, r) - matmul(rot, r))
        ! cubic:    9.2246936878837005E-004  -1.2421466732234876E-002   5.0903075486417038E-003
        ! quartic:  1.8213122621091316E-004  -1.8431192216894488E-004  -5.0147631935715253E-004
        ! quintic: -1.0719543411141785E-004   8.4190997773703202E-004  -1.9197702022211161E-004

    et = (129600.0_wp + 86400.0_wp) / 2.0_wp
    call moon_pa%j2000_to_frame(et, rot)
    call from_j2000_to_moon_pa(real(et, real64), rot_true)
    write(*,*) ''
    write(*,*) 'rot error at et', rot_true - rot
    write(*,*) 'pos error: ', norm2(matmul(rot_true, r) - matmul(rot, r))
        ! quartic:   2.3932315798447235E-005   7.9659319212055379E-005  -7.2767983567700867E-005

    write(*,*) ''
    write(*,*) '-------'
    write(*,*) ' comprehensive test'
    write(*,*) '-------'
    write(*,*) ''
    et = 0.0_wp
    max_r_error = 0.0_wp
    do
        et = et + 6.0_wp *3600.0_wp
        if (et > 3187296000.0_wp) exit
        call moon_pa%j2000_to_frame(et, rot)          ! splined version
        call from_j2000_to_moon_pa(real(et, real64), rot_true)      ! true version from spice : max error: 5.5664815594496319E-004 km
        ! call from_j2000_to_iau_moon(et, rot_true)   ! compare to iau_moon :     max error: 0.98148100150030848 km
        r_error = norm2(matmul(rot_true, r) - matmul(rot, r))
        if (r_error > max_r_error) max_r_error = r_error
    end do
    write(*,*) 'max r error over entire time range: ', max_r_error
    write(*,*) ''

    write(*,*) ''
    write(*,*) '-------'
    write(*,*) ' timing test'
    write(*,*) '-------'
    write(*,*) ''
    call cpu_time(tstart)
    et = 0.0_wp
    do
        et = et + 6.0_wp *3600.0_wp
        if (et > 3187296000.0_wp) exit
        call from_j2000_to_moon_pa(real(et, real64), rot_true)
    end do
    call cpu_time(tend)
    write(*,'(A,F6.3,A)') 'spice time:  ', (tend-tstart), 'sec'
    call cpu_time(tstart)
    et = 0.0_wp
    do
        et = et + 6.0_wp *3600.0_wp
        if (et > 3187296000.0_wp) exit
        call moon_pa%j2000_to_frame(et, rot)
    end do
    call cpu_time(tend)
    write(*,'(A,F6.3,A)') 'spline time: ', (tend-tstart), 'sec'
    write(*,*) ''

    contains

    pure function unit(r) result(u)
        real(wp),dimension(:),intent(in) :: r
        real(wp),dimension(size(r))      :: u
        real(wp) :: rmag
        rmag = norm2(r)
        if (rmag==0.0_wp) then
            u = 0.0_wp
        else
            u = r / rmag
        end if
    end function unit

#else
    error stop 'Error: SPICE not available.'
#endif

end program test