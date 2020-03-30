module eos_wate_module
  !! Equation of state for non-isothermal water and air NCG.

#include <petsc/finclude/petscsys.h>

  use petscsys
  use eos_wgte_module
  use ncg_air_thermodynamics_module

  implicit none
  private

  type, public, extends(eos_wgte_type) :: eos_wate_type
     !! Equation of state object for non-isothermal water and air NCG.
   contains
     private
     procedure, public :: init => eos_wate_init
     procedure, public :: destroy => eos_wate_destroy
  end type eos_wate_type

contains

!------------------------------------------------------------------------

  subroutine eos_wate_init(self, json, thermo, logfile)
    !! Initialise eos_wate object.

    use fson
    use fson_mpi_module, only: fson_get_mpi, fson_has_mpi
    use logfile_module
    use thermodynamics_module

    class(eos_wate_type), intent(in out) :: self
    type(fson_value), pointer, intent(in) :: json !! JSON input object
    class(thermodynamics_type), intent(in), target :: thermo !! Thermodynamics object
    type(logfile_type), intent(in out), optional :: logfile
    ! Locals:
    PetscReal :: air_partial_pressure_scale

    call self%eos_wgte_type%init(json, thermo, logfile)

    self%name = "wate"
    self%description = "Water, air NCG, tracer and energy"
    self%primary_variable_names(3) = "air partial pressure"
    self%component_names(2) = "air"
    self%required_output_fluid_fields = [ &
         "pressure                   ", "temperature                ", &
         "region                     ", "air_partial_pressure       ", &
         "liquid_tracer_mass_fraction", "vapour_saturation          "]
    self%default_output_fluid_fields = [ &
         "pressure                   ", "temperature                ", &
         "region                     ", "air_partial_pressure       ", &
         "liquid_tracer_mass_fraction", "vapour_saturation          "]

    if (fson_has_mpi(json, "eos.primary.scale.air_partial_pressure")) then
       call fson_get_mpi(json, "eos.primary.scale.air_partial_pressure", &
         val = air_partial_pressure_scale)
       self%primary_scale(3, :) = air_partial_pressure_scale
    end if

    allocate(ncg_air_thermodynamics_type :: self%gas)
    call self%gas%init()

  end subroutine eos_wate_init

!------------------------------------------------------------------------

  subroutine eos_wate_destroy(self)
    !! Destroy eos_wate object.

    class(eos_wate_type), intent(in out) :: self

    deallocate(self%gas)
    call self%eos_wgte_type%destroy()

  end subroutine eos_wate_destroy

!------------------------------------------------------------------------

end module eos_wate_module
