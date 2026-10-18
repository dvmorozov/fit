// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The profile the scientific module's shapes decompose: the angles of
a diffraction pattern, and '.DAT' read as one.)

APART FROM THE FRONT DOOR because the front door checks that the module's
curve types linked (curve_type_registration), which links the user's formula
and through it the LCL - and the plain-FPC suite, which tests the axes and the
formats, links no LCL. Everything here is what that suite can reach.
}
unit diffraction_profile;

{$mode objfpc}{$H+}

interface

{ Registers the diffraction angles as axis modes and '.DAT' as a diffraction
  profile, read by the framework's two-column reader. Idempotent. Before the
  framework's own formats (RegisterAllDataLoaders), so a file dialog lists it
  first, as it always has. }
procedure RegisterDiffractionProfile;

implementation

uses
    diffraction_axis_modes, data_loader_registry, dat_file_loader;

procedure RegisterDiffractionProfile;
begin
    RegisterDiffractionAxisModes;
    //  NO COORDINATES DECLARED, although the format is named for diffraction:
    //  '.dat' is what every two-column series is saved as, and declaring 2
    //  Theta here captioned a price series in scattering angle. A diffraction
    //  model says so itself (the peak shapes' PreferredAxisMode). Once: a
    //  second claim of '.DAT' is refused.
    if not Assigned(FindDataLoaderClass('profile.dat')) then
        RegisterDataLoader(TDATFileLoader, '.DAT', 'Diffraction profile');
end;

end.
