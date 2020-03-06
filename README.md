# hask-marker

## Concepts 

### Layering context over data objects

```
DepartmentUser
    Department
        id
    PermissionUser
        [Permissions]
        User
            id
```

On each layer we get specific data from corresponding data layer and use it for scenario. When we pass conditions, or finish process - pass nested context in nested layer logic.