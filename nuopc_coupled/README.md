# EMSF Coupled Example Description

This example demonstrates a simple two-component Earth System Model (ESM) application using ESMF and NUOPC, coupling an atmospheric model (ATM) with an ocean model (OCN) via a connector.

---

## File Overview

### **1. `esmfApp.F90`**
- Main program that initializes and runs the ESM application.
- Responsibilities:
  - Call `ESMF_Initialize` and `ESMF_Finalize`.
  - Create the top-level ESM component (`esmComp`) using `ESMF_GridCompCreate`.
  - Set the NUOPC `SetServices` routine for the component.
  - Call `ESMF_GridCompInitialize`, `ESMF_GridCompRun`, and `ESMF_GridCompFinalize`.
  - Destroy the ESM component.
- Acts as the **entry point** of the application.

---

### **2. `esmf.F90`**
- Module that specializes the generic ESM component.
- Responsibilities:
  - Provides the `SetServices` routine for the driver component.
  - Derives the driver from `NUOPC_Driver`.
  - Specializes the driver with the `SetModelServices` routine.
  - Adds child components (ATM and OCN) to the driver.
  - Adds connectors between ATM → OCN and OCN → ATM.
  - Sets up the application clock with start/stop time and time step.

---

### **3. `atm.F90`**
- Atmospheric model component.
- Responsibilities:
  - Provides `SetServices` routine to derive from `NUOPC_Model` and specialize `Advertise`, `RealizeProvided`, and `Advance` routines.
  - **Advertise**:
    - Importable field: `sea_surface_temperature` (sst)
    - Exportable fields: `air_pressure_at_sea_level` (pmsl), `surface_net_downward_shortwave_flux` (rsns)
  - **Realize**:
    - Reads configuration from `2comp_time_example.nml` for grid dimensions and coordinates.
    - Creates `ESMF_Grid` objects for input and output fields.
    - Creates and realizes the importable and exportable fields.
  - **Advance**:
    - Moves the model forward in time according to its internal clock.
    - Logs current and next time steps.

---

### **4. `ocn.F90`**
- Ocean model component.
- Responsibilities:
  - Provides `SetServices` routine to derive from `NUOPC_Model` and specialize `Advertise`, `RealizeProvided`, `SetClock`, and `Advance` routines.
  - **Advertise**:
    - Importable fields: `air_pressure_at_sea_level` (pmsl), `surface_net_downward_shortwave_flux` (rsns)
    - Exportable field: `sea_surface_temperature` (sst)
  - **Realize**:
    - Reads configuration from `2comp_time_example.nml`.
    - Creates `ESMF_Grid` objects for input and output fields.
    - Creates and realizes importable and exportable fields.
  - **SetClock**:
    - Initializes the internal clock with a stability time step (5 minutes in the example).
  - **Advance**:
    - Moves the ocean model forward in time according to its internal clock.
    - Logs current and next time steps.

---

## Summary of Data Flow

- ATM exports `air_pressure_at_sea_level` and `surface_net_downward_shortwave_flux`.
- OCN imports these fields from ATM.
- OCN exports `sea_surface_temperature`.
- ATM imports `sea_surface_temperature` from OCN.
- Connector components (`cplSS`) handle the mapping of fields between ATM and OCN grids.

---

## Notes

- All components are derived from NUOPC base classes (`NUOPC_Model`, `NUOPC_Driver`).
- Fields are created with `ESMF_Grid` objects specifying the spatial domain.
- The time step for each component can be controlled via the internal clock.
- You can enable/disable import fields or connectors by commenting/uncommenting the corresponding macros (`WITHIMPORTFIELDS`, `WITHCONNECTORS`).

