// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one resource name by which a module keeps its own state in a
project file.)

WHY A UNIT FOR ONE CONSTANT. Both sides spell it: the framework asks each module
for '<module>/project-state' when a project is saved and posts it back when one
is opened, and the module answers that resource in its own TryGet/TryPost. If the
two spelled it differently, saving would collect nothing and restoring would
reach nobody - and neither half would report anything, because a resource nobody
owns is an ordinary "not mine" answer. That is the same failure rest_polling
exists to prevent for the polled routes, and it is prevented the same way.

WHY IT IS A RESOURCE AND NOT A NEW INTERFACE MEMBER. IModuleSession already
answers named resources carrying JSON the module defines. A module that keeps
nothing simply does not declare this one, and the framework asks only the modules
whose declared resources include it - so a module with no state is not an error
and needs no code at all.

THE FRAMEWORK NEVER PARSES WHAT COMES BACK. It stores the JSON under the module's
name and hands it back verbatim, which is what lets a module extend the project
file without the framework naming a module.
}
unit module_project_state;

{$mode objfpc}{$H+}

interface

const
    { The resource, without the module prefix: a module declares
      '<its name>/project-state' among its resources and answers it. }
    ProjectStateResource = 'project-state';

implementation

end.
