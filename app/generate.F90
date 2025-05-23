!***************************************************************************
!>
!  Main program to generate the data file that can be used
!  to interpolate the moon frames.

program generate

    use moon_frame_module
    use bspline_module
    use iso_fortran_env, only: wp => real64 !! spice uses double precision
    use moon_frame_spice_interface

    implicit none

#ifdef HAS_SPICELIB

    abstract interface
        subroutine j2000_to_frame_func (et, rot)
            import :: wp
            implicit none
            real(wp), intent(in) :: et
            real(wp), intent(out) :: rot(3, 3)
        end subroutine j2000_to_frame_func
    end interface

    ! load the kernels:
    call furnsh ( './kernels/moon_de440_250416.tf' )
    call furnsh ( './kernels/moon_pa_de440_200625.bpc'   )
    call furnsh ( './kernels/pck00011.tpc'   )
    call furnsh ( './kernels/naif0012.tls'   )

    !call test()  ! test

    call generate_csv_file('data/moon_pa_2000_2100.csv', from_j2000_to_moon_pa)
    call generate_csv_file('data/moon_me_2000_2100.csv', from_j2000_to_moon_me)  ! really this is a constant rotation from moon_pa and could be computed using that formula.
    call generate_csv_file('data/iau_moon_2000_2100.csv', from_j2000_to_iau_moon) ! also a simple formula

    contains

    subroutine generate_csv_file(filename, func)
        !! generate a csv file with the roll, pitch, and yaw angles (in radians)
        !! to transform from j2000 to the given frame.

        character(len=*), intent(in) :: filename
        procedure(j2000_to_frame_func) :: func

        real(wp),parameter :: dt = 12.0_wp * 3600.0_wp !! 12 hr step

        real(wp) :: et0, etf, et, rot(3,3), roll,  pitch,  yaw
        integer :: istat, iunit

        ! generate roll/patch/yaw of the moon_pa frame (2000-2100)
        call str2et('2000 Jan 1, 12:00:00 TDB', et0)
        call str2et('2101 Jan 1, 12:00:00 TDB', etf)

        !open file:
        open(newunit=iunit, file=filename, status='replace', iostat=istat)
        if (istat /= 0) error stop 'Error opening file'

        write(iunit,'(A15,3(",",A27))') 'et (sec)', 'roll (rad)', 'pitch (rad)', 'yaw (rad)'
        et = et0
        do
            call func(et, rot)
            !call m2eul(rot, 1, 2, 3, roll, pitch, yaw) ! test
            call rot_to_rpy(rot, roll, pitch, yaw)
            write(iunit,'(F15.1, 3(",",E27.17))') et, roll,  pitch,  yaw
            et = et + dt
            if (et > etf) exit
        end do

        close(iunit, iostat=istat)

    end subroutine generate_csv_file

    pure subroutine rot_to_rpy(r, roll, pitch, yaw)
        !! rotation matrix to roll, pitch, yaw
        real(wp), intent(in)  :: r(3,3)
        real(wp), intent(out) :: roll, pitch, yaw !! rad
        ! Handle gimbal lock
        if (abs(r(3,1)) < (1.0_wp - 1e-12_wp)) then
            pitch = -asin(r(3,1))
            roll  = atan2(r(3,2), r(3,3))
            yaw   = atan2(r(2,1), r(1,1))
        else
            ! Gimbal lock: pitch is +/-90 deg
            pitch = -asin(r(3,1))
            roll  = 0.0_wp
            if (r(3,1) < 0.0_wp) then
                yaw = atan2(-r(1,2), r(2,2))
            else
                yaw = atan2(r(1,2), r(2,2))
            end if
        end if
    end subroutine rot_to_rpy

#else
    error stop 'Error: SPICE not available.'
#endif

end program generate