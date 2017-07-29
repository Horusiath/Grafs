/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace Grafs

open System

/// Common base class for all GraphQL related exceptions.
type GQLException(msg) = inherit Exception(msg)

/// Common base class for all GraphQL related exceptions.
type MalformedQueryException(msg) = inherit GQLException(msg)