!***************************************************************************
!>
!  Module for interpolation of body-fixed Moon frames.

module moon_frame_module
    use bspline_module
    use iso_fortran_env

    implicit none

    private

#ifdef REAL32
    integer,parameter :: wp = real32   !! Real working precision [4 bytes]
#elif REAL64
    integer,parameter :: wp = real64   !! Real working precision [8 bytes]
#elif REAL128
    integer,parameter :: wp = real128  !! Real working precision [16 bytes]
#else
    integer,parameter :: wp = real64   !! Real working precision if not specified [8 bytes]
#endif
    integer,parameter,public :: moon_frame_wp = wp  !! working precision for moon frame

    real(wp),parameter :: rad2deg = 180.0_wp / acos(-1.0_wp)
    real(wp),parameter :: deg2rad = 1.0_wp / rad2deg

    type,public :: moon_frame_interpolater
        !! Main class to read a pre-computed CSV file with roll, pitch, and yaw angles
        !! and interpolate the angles to get the rotation matrix for a given ephemeris time.
        private
        type(bspline_1d) :: roll_spline
        type(bspline_1d) :: pitch_spline
        type(bspline_1d) :: yaw_x_spline  !! x-component of yaw angle unit vector: x=cos(yaw)
        type(bspline_1d) :: yaw_y_spline  !! y-component of yaw angle unit vector: y=sin(yaw)
    contains
        private
        procedure,public :: initialize => initialize_moon_frame_interpolater
        procedure,public :: destroy    => destroy_moon_frame_interpolater
        procedure,public :: j2000_to_frame
    end type moon_frame_interpolater

contains

    subroutine initialize_moon_frame_interpolater(me, filename, k, extrapolate)
        !! initialize the moon frame interpolater with the given csv file.

        use csv_module

        class(moon_frame_interpolater),intent(inout) :: me
        character(len=*),intent(in) :: filename !! csv file with roll, pitch, and yaw angles vs ephemeris time. (see [[generate_csv_file]])
        integer,intent(in),optional :: k !! spline order (`kx` in bspline_module). If not given, use the default quartic order.
        logical,intent(in),optional :: extrapolate !! if true, extrapolate the spline outside the range of the data. Default is false.

        logical :: status_ok
        type(csv_file) :: f
        real(wp),dimension(:),allocatable :: et, roll, pitch, yaw, yaw_x, yaw_y
        integer :: iflag, i, kx
        logical :: extrap !! extrapolate flag

        ! optional arguments:
        if (present(k)) then
            kx = k
        else
            kx = bspline_order_quartic
        end if
        if (present(extrapolate)) then
            extrap = extrapolate
        else
            extrap = .false.
        end if

        ! read the data from the csv file:
        call f%read(filename,header_row=1,status_ok=status_ok)
        if (.not. status_ok) error stop 'error reading file: '//trim(filename)

        ! get data
        call f%get(1,et, status_ok);   if (.not. status_ok) error stop 'error getting et from file: '//trim(filename)
        call f%get(2,roll, status_ok); if (.not. status_ok) error stop 'error getting roll from file: '//trim(filename)
        call f%get(3,pitch,status_ok); if (.not. status_ok) error stop 'error getting pitch from file: '//trim(filename)
        call f%get(4,yaw,  status_ok); if (.not. status_ok) error stop 'error getting yaw from file: '//trim(filename)
        call f%destroy()

        ! for the moon frames, roll and pitch have no discontinuities,
        ! so they can be splined normally. Yaw will go from 0 to 2pi,
        ! so we need to convert the angle to its vector components and spline those,
        ! and then convert back to angle when we need it.
        allocate(yaw_x(size(yaw)))
        allocate(yaw_y(size(yaw)))
        do i = 1, size(yaw)
            yaw_x(i) = cos(yaw(i))
            yaw_y(i) = sin(yaw(i))
        end do

        ! initialize the splines:
        call me%roll_spline%initialize (et, roll, kx,  iflag, extrap=extrap); if (iflag/=0) error stop 'error initializing roll spline: '//trim(filename)
        call me%pitch_spline%initialize(et, pitch, kx, iflag, extrap=extrap); if (iflag/=0) error stop 'error initializing pitch spline: '//trim(filename)
        call me%yaw_x_spline%initialize(et, yaw_x, kx, iflag, extrap=extrap); if (iflag/=0) error stop 'error initializing yaw_x spline: '//trim(filename)
        call me%yaw_y_spline%initialize(et, yaw_y, kx, iflag, extrap=extrap); if (iflag/=0) error stop 'error initializing yaw_y spline: '//trim(filename)

    end subroutine initialize_moon_frame_interpolater

    subroutine j2000_to_frame(me, et, rot)
        class(moon_frame_interpolater), intent(inout) :: me
        real(wp), intent(in) :: et
        real(wp), intent(out) :: rot(3, 3)

        real(wp) :: roll, pitch, yaw_x, yaw_y, yaw
        integer :: iflag

        call me%roll_spline%evaluate (et, 0, roll, iflag); if (iflag/=0) error stop 'error computing roll spline.'
        call me%pitch_spline%evaluate(et, 0, pitch, iflag); if (iflag/=0) error stop 'error computing pitch spline.'
        call me%yaw_x_spline%evaluate(et, 0, yaw_x, iflag); if (iflag/=0) error stop 'error computing yaw_x spline.'
        call me%yaw_y_spline%evaluate(et, 0, yaw_y, iflag); if (iflag/=0) error stop 'error computing yaw_y spline.'
        yaw = atan2(yaw_y, yaw_x)

        ! convert to rotation matrix:
        call rpy_to_rot(roll, pitch, yaw, rot)

    end subroutine j2000_to_frame

    pure subroutine rpy_to_rot(roll, pitch, yaw, r)
        !! roll, patch, yaw to rotation matrix
        real(wp), intent(in)  :: roll, pitch, yaw !! rad
        real(wp), intent(out) :: r(3,3)
        real(wp) :: cr, sr, cp, sp, cy, sy

        cr = cos(roll)
        sr = sin(roll)
        cp = cos(pitch)
        sp = sin(pitch)
        cy = cos(yaw)
        sy = sin(yaw)

        r(1,1) = cy*cp
        r(1,2) = cy*sp*sr - sy*cr
        r(1,3) = cy*sp*cr + sy*sr
        r(2,1) = sy*cp
        r(2,2) = sy*sp*sr + cy*cr
        r(2,3) = sy*sp*cr - cy*sr
        r(3,1) = -sp
        r(3,2) = cp*sr
        r(3,3) = cp*cr
    end subroutine rpy_to_rot

    subroutine destroy_moon_frame_interpolater(me)
        class(moon_frame_interpolater), intent(inout) :: me
        call me%roll_spline%destroy()
        call me%pitch_spline%destroy()
        call me%yaw_x_spline%destroy()
        call me%yaw_y_spline%destroy()
    end subroutine destroy_moon_frame_interpolater

end module moon_frame_module