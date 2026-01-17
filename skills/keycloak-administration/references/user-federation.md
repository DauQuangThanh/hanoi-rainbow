# User Federation

## LDAP/Active Directory Integration

**Purpose:** Sync users from existing directory services

### Configure LDAP

1. User Federation → Add Provider → LDAP
2. Edit mode: READ_ONLY, WRITABLE, or UNSYNCED
3. Vendor: Active Directory, Red Hat Directory Server, Other

### Connection Settings

- Connection URL: `ldap://ldap.example.com:389` or `ldaps://ldap.example.com:636`
- Enable StartTLS: For secure connection on port 389
- Bind DN: `cn=admin,dc=example,dc=com`
- Bind credential: LDAP admin password

### LDAP Search Settings

- Users DN: `ou=users,dc=example,dc=com`
- Username LDAP attribute: `uid` or `sAMAccountName` (AD)
- RDN LDAP attribute: `uid` or `cn`
- UUID LDAP attribute: `entryUUID` or `objectGUID` (AD)
- User object classes: `inetOrgPerson, organizationalPerson`

### Sync Settings

- Sync registrations: Allow creating LDAP users from KeyCloak
- Import users: Full sync or on-demand (login)
- Periodic full sync: Scheduled synchronization
- Changed users sync: Incremental sync

### Active Directory Specific

- Edit mode: WRITABLE (to enable password changes)
- Vendor: Active Directory
- Username attribute: `sAMAccountName`
- UUID attribute: `objectGUID`
- User object classes: `person, organizationalPerson, user`

### LDAP Mappers

- **User attribute**: Map LDAP attributes to KeyCloak attributes
- **Full name**: Map `cn` to first/last name
- **Group**: Import LDAP groups
- **Role**: Map LDAP groups to KeyCloak roles

## Custom User Federation

**Purpose:** Integrate with custom user databases or APIs

### Implementation

1. Implement `UserStorageProvider` interface (Java)
2. Package as JAR and deploy to KeyCloak
3. Configure provider in User Federation

### Use Cases

- Legacy user databases
- Custom authentication systems
- Third-party user services
- Special validation requirements

## User Federation Best Practices

- Use READ_ONLY mode for production LDAP unless password writeback required
- Enable periodic sync for up-to-date user information
- Configure LDAPS or StartTLS for secure connections
- Map only required LDAP attributes
- Test LDAP connection before enabling
- Monitor sync errors in server logs
- Document LDAP/AD schema mappings
- Plan for LDAP downtime scenarios
