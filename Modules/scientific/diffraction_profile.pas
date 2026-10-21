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

{ Registers the diffraction angles as axis modes, '.DAT' as a diffraction
  profile read by the framework's two-column reader, and the diffractometers'
  own formats (diffraction_formats). Idempotent. Before the
  framework's own formats (RegisterAllDataLoaders), so a file dialog lists it
  first, as it always has. }
procedure RegisterDiffractionProfile;

implementation

uses
    diffraction_axis_modes, data_loader_registry, dat_file_loader,
    diffraction_formats;

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
    //  THE DIFFRACTOMETERS' OWN FORMATS (2026-10-20), and these DO declare
    //  their coordinates: unlike '.dat', a file of any of them is a powder
    //  pattern, 2 Theta against intensity, whatever else could be saved so.
    //  What each reads, and what it leaves, is diffraction_formats'.
    RegisterDataLoader(TXrdmlLoader, '.XRDML', 'PANalytical XRDML scan',
        TwoThetaAxisModeId, IntensityAxisModeId);
    RegisterDataLoader(TUxdLoader, '.UXD', 'Bruker UXD scan',
        TwoThetaAxisModeId, IntensityAxisModeId);
    RegisterDataLoader(TGsasLoader, '.GSA;.GSS;.FXYE;.GSAS',
        'GSAS powder data', TwoThetaAxisModeId, IntensityAxisModeId);
    RegisterDataLoader(TPowderCifLoader, '.CIF', 'Powder pattern CIF',
        TwoThetaAxisModeId, IntensityAxisModeId);
    //  Plain columns with an esd beside them (.xye), and Fit2D's integrated
    //  pattern (.chi): the two-column reader's shape, with an angle first.
    RegisterDataLoader(TDATFileLoader, '.XYE;.CHI', 'Powder pattern columns',
        TwoThetaAxisModeId, IntensityAxisModeId);
    //  What each holds, in a line: what the data catalog says beside it.
    DescribeDataFormat('Diffraction profile', 'a diffraction pattern as two ' +
        'columns, angle and intensity');
    DescribeDataFormat('PANalytical XRDML scan', 'a PANalytical ' +
        'diffractometer''s scan: 2 Theta against counts');
    DescribeDataFormat('Bruker UXD scan', 'a Bruker diffractometer''s scan ' +
        'exported as text: 2 Theta against counts');
    DescribeDataFormat('GSAS powder data', 'a powder pattern prepared for ' +
        'refinement in GSAS: 2 Theta against intensity');
    DescribeDataFormat('Powder pattern CIF', 'a measured powder pattern ' +
        'published as a CIF; a crystal structure''s CIF is refused');
    DescribeDataFormat('Powder pattern columns', 'an integrated or reduced ' +
        'powder pattern: 2 Theta, intensity and its error');
end;

end.
